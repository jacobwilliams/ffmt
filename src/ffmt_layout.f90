module ffmt_layout
!! Re-indents a tokenized file: statements by their nesting depth, continuation lines by
!! alignment or a fixed indent, and comment lines like the code around them. Trailing blanks
!! are removed. Nothing else within a line is changed.
!!
!! Items protected by formatter directives (see [[ffmt_directives]]) are written as they are.
use ffmt_tokens
use ffmt_statements
use ffmt_constructs, only: stmt_info_t
use ffmt_options
use ffmt_whitespace, only: GAP_KEEP, GAP_MIN1
use ffmt_case, only: CASE_LOWER, CASE_UPPER
use ffmt_strings, only: string_builder_t, utf8_width
use ffmt_directives, only: protected_items
implicit none
private

public :: layout


integer, parameter :: MAX_BRACKETS = 256

type :: writer_t
    !! Output being built, and the alignment state of the current statement.
    type(string_builder_t) :: buf
    integer :: col = 0                          !! output column: characters since the last line break
    integer :: nbrackets = 0                    !! open brackets in the current statement
    integer :: bracket_first(MAX_BRACKETS) = -1 !! column of the first item after each open bracket, or -1
    integer :: anchor_col(3) = -1               !! column of the first item after an assignment `=` / `=>`,
                                                !! a `use` statement's `only:` and a declaration's `::`, or -1
    integer :: pending_anchor = 0               !! the next code token is the first item after this anchor
    logical :: pending_bracket = .false.        !! the next code token is the first item after a bracket
    logical :: separated = .false.              !! a top-level `,` or `:` has been seen in the statement
    character(len=:), allocatable :: prev       !! lowercase text of the previous code token
    integer :: comment_in = -1                  !! input column of the comment on the previous line, or -1
    integer :: comment_out = -1                 !! output column of that comment
    logical :: line_comment = .false.           !! a comment has been written on the current line
    integer :: stmt_in = -1                     !! input column of the last statement, while comment lines
                                                !! directly follow it, or -1
    integer :: stmt_out = -1                    !! output column of that statement
    logical :: generic = .false.                !! the current statement is a `generic` binding
    logical :: declaration = .false.            !! a top-level `::` has been seen in the current statement
    integer :: line_length = 0                  !! maximum line length, or 0
    logical :: keep_original = .true.           !! fall back to a code line's original indentation when the
                                                !! new one would make it too long (when lines are not split)
    integer, allocatable :: long_lines(:)       !! input lines that kept their indentation to stay within the line length
    integer, allocatable :: gap(:)              !! blanks wanted before each token (see [[compute_gaps]])
    integer, allocatable :: case_mode(:)        !! letter case wanted for each token (see [[compute_case]])
end type writer_t

type :: piece_t
    !! A piece of an output line: a token, or the blanks before one.
    character(len=:), allocatable :: text
    integer :: tok = 0                          !! token index, or 0 for blanks
end type piece_t

! anchors for aligning continuation lines outside brackets, in order of preference
integer, parameter :: ANCHOR_ASSIGN = 1, ANCHOR_ONLY = 2, ANCHOR_DECL = 3

contains

function layout(src, tokens, stmts, info, depth, units, procedures, gap, case_mode, opts, long_lines) result(out)
    !! The formatted text. `info`, `depth`, `units` and `procedures` come from [[analyze_structure]],
    !! `gap` from [[compute_gaps]] and `case_mode` from [[compute_case]].
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_list_t), intent(in) :: stmts
    type(stmt_info_t), intent(in) :: info(:)
    integer, intent(in) :: depth(:)     !! nesting depth of each item
    integer, intent(in) :: units(:)     !! how many of the enclosing scopes are program units
    integer, intent(in) :: procedures(:) !! how many of the enclosing scopes are procedures
    integer, intent(in) :: gap(:)       !! blanks wanted before each token
    integer, intent(in) :: case_mode(:) !! letter case wanted for each token
    type(format_options_t), intent(in) :: opts
    integer, allocatable, intent(out), optional :: long_lines(:)   !! input lines that kept their indentation
                                                                   !! because the new one would make them longer
                                                                   !! than `opts%line_length`
    character(len=:), allocatable :: out

    type(writer_t) :: w
    integer :: k, level
    logical, allocatable :: keep(:)

    w%line_length = opts%line_length
    w%keep_original = .not. opts%split_lines
    w%gap = gap
    w%case_mode = case_mode
    allocate (w%long_lines(0))
    keep = protected_items(src, tokens, stmts)
    do k = 1, stmts%n
        level = depth(k)
        if (.not. opts%indent_units) level = level - units(k)
        if (.not. opts%indent_procedures) level = level - procedures(k)
        level = max(level, 0)
        associate (s => stmts%s(k))
            if (keep(k)) then                   ! a formatter directive: written as it is
                call emit_range(w, src, tokens, s%first, s%last)
                w%comment_in = -1
                w%stmt_in = -1
                cycle
            end if
            select case (s%kind)
            case (SK_BLANK)
                call emit_blank(w, src, tokens, s)
                w%comment_in = -1
                w%stmt_in = -1
            case (SK_PREPROC)
                call emit_range(w, src, tokens, s%first, s%last)
                w%comment_in = -1
            case (SK_COMMENT)
                call emit_comment_line(w, src, tokens, s, opts%base_indent + level*opts%indent, opts)
            case (SK_CODE)
                call emit_statement(w, src, tokens, s, info(k)%label > 0, opts%base_indent + level*opts%indent, opts)
            end select
        end associate
    end do
    out = w%buf%get()
    if (present(long_lines)) long_lines = w%long_lines
end function layout

integer function fitting_column(w, src, tokens, f, lend, candidates, from, prefix, always_original) result(col)
    !! The first of the `candidates` columns at which the output line fits within the line length, or
    !! the smallest if none fits. The last candidate is the line's original column: if only that one
    !! fits, the line is recorded in `long_lines`. When lines are split (`w%keep_original` false), the
    !! original column is not a candidate for code, since splitting will make the line fit; it is
    !! still used if `always_original` is true (for comments, which are not split). Also when lines are
    !! split, a candidate other than the last is accepted if the line can be split to fit there: if
    !! its first piece up to a possible break fits.
    !!
    !! The input line starts with token `f`. The output line is `prefix` characters (e.g. `& ` for a
    !! leading continuation marker) followed by tokens `from:lend` (default `f:lend`).
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: f, lend
    integer, intent(in) :: candidates(:)
    integer, intent(in), optional :: from, prefix
    logical, intent(in), optional :: always_original
    integer :: k, width, ncand, first_piece
    logical :: original

    col = max(0, candidates(1))
    if (w%line_length <= 0) return
    original = w%keep_original
    if (present(always_original)) original = original .or. always_original
    ncand = size(candidates)
    if (.not. original .and. ncand > 1) ncand = ncand - 1
    if (present(from)) then
        width = prefix + line_width(w, src, tokens, from, lend)
    else
        width = line_width(w, src, tokens, f, lend)
    end if
    first_piece = -1
    do k = 1, ncand
        if (candidates(k) < 0) cycle
        if (.not. original .and. k < ncand .and. candidates(k) + width > w%line_length) then
            if (first_piece < 0) then
                if (present(from)) then
                    first_piece = prefix + first_piece_width(w, src, tokens, from, lend)
                else
                    first_piece = first_piece_width(w, src, tokens, f, lend)
                end if
            end if
            if (candidates(k) + first_piece + 2 <= w%line_length) then   ! ` &` after the first piece
                col = candidates(k)
                return
            end if
        end if
        if (candidates(k) + width <= w%line_length) then
            col = candidates(k)
            if (original .and. k == size(candidates) .and. k > 1 .and. col /= candidates(1)) &
                w%long_lines = [w%long_lines, tokens%tok(f)%line]
            return
        end if
    end do
    col = max(0, minval(candidates(1:ncand)))     ! too long: it is split later, or was too long already
end function fitting_column

integer function first_piece_width(w, src, tokens, first, lend) result(width)
    !! Width of tokens `first:lend` up to the end of the first code token that has another code token
    !! after it on the line (the shortest first part of a split), or a huge value if there is none.
    type(writer_t), intent(in) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: first, lend
    type(piece_t), allocatable :: pieces(:)
    integer :: k, m, total

    width = huge(1)/2
    call line_pieces(w, src, tokens, first, lend, pieces)
    total = 0
    do k = 1, size(pieces)
        if (scan(pieces(k)%text, achar(10)//achar(13)) > 0) return
        total = total + utf8_width(pieces(k)%text)
        if (pieces(k)%tok == 0) cycle
        if (.not. is_code_kind(tokens%tok(pieces(k)%tok)%kind)) cycle
        if (pieces(k)%tok == first .and. tokens%tok(first)%kind == TK_INT) cycle   ! a statement label
        do m = k + 1, size(pieces)
            if (pieces(m)%tok == 0) cycle
            if (is_code_kind(tokens%tok(pieces(m)%tok)%kind) .and. .not. tokens%tok(pieces(m)%tok)%glued) then
                width = total
                return
            end if
            exit
        end do
    end do
end function first_piece_width

integer function line_width(w, src, tokens, first, lend) result(width)
    !! Width of tokens `first:lend` as they will be written on one line, without the line break and
    !! without a trailing comment (compilers only limit the length of the code). A comment on a line
    !! of its own counts.
    type(writer_t), intent(in) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: first, lend
    type(piece_t), allocatable :: pieces(:)
    integer :: k, j, before_blank

    call line_pieces(w, src, tokens, first, lend, pieces)
    width = 0
    before_blank = 0
    do k = 1, size(pieces)
        if (pieces(k)%tok > 0 .and. k > 1) then
            if (tokens%tok(pieces(k)%tok)%kind == TK_COMMENT) then
                width = before_blank            ! a trailing comment, and the blanks before it
                return
            end if
        end if
        if (pieces(k)%tok > 0) before_blank = width + utf8_width(pieces(k)%text)
        j = scan(pieces(k)%text, achar(10)//achar(13))
        if (j > 0) then                       ! a line break, or a token continued on the next line
            width = width + utf8_width(pieces(k)%text(1:j - 1))
            return
        end if
        width = width + utf8_width(pieces(k)%text)
    end do
end function line_width

subroutine line_pieces(w, src, tokens, first, lend, pieces)
    !! The pieces of output for tokens `first:lend` of a line: the tokens, with the blanks between them
    !! adjusted by the whitespace rules and trailing blanks removed.
    type(writer_t), intent(in) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: first, lend
    type(piece_t), allocatable, intent(out) :: pieces(:)
    integer :: t, n, g
    character(len=:), allocatable :: text

    allocate (pieces(2*(lend - first + 1) + 1))
    n = 0
    do t = first, lend
        if (tokens%tok(t)%kind == TK_SPACE) then
            if (t == lend) cycle
            if (tokens%tok(t + 1)%kind == TK_NEWLINE) cycle
            text = token_text(src, tokens%tok(t))
            ! blanks between two code tokens may be adjusted by the rule for the second one
            if (t > first .and. t < lend) then
                if (is_code_kind(tokens%tok(t - 1)%kind) .and. is_code_kind(tokens%tok(t + 1)%kind)) then
                    g = w%gap(t + 1)
                    if (g == 0) cycle
                    if (g > 0) text = repeat(' ', g)
                end if
            end if
            call add(text, 0)
            cycle
        end if
        if (t > first .and. is_code_kind(tokens%tok(t)%kind)) then
            if (is_code_kind(tokens%tok(t - 1)%kind)) then     ! no blank in the input
                g = w%gap(t)
                if (g == GAP_MIN1) then
                    call add(' ', 0)
                else if (g > 0) then
                    call add(repeat(' ', g), 0)
                end if
            end if
        end if
        text = token_text(src, tokens%tok(t))
        if (tokens%tok(t)%kind == TK_COMMENT) text = trim_blanks(text)
        select case (w%case_mode(t))
        case (CASE_LOWER)
            text = to_case(text, .false.)
        case (CASE_UPPER)
            text = to_case(text, .true.)
        end select
        call add(text, t)
    end do
    pieces = pieces(1:n)

contains

    subroutine add(text, tok)
        character(len=*), intent(in) :: text
        integer, intent(in) :: tok
        n = n + 1
        pieces(n)%text = text
        pieces(n)%tok = tok
    end subroutine add

end subroutine line_pieces

subroutine put(w, text)
    !! Append `text` and track the output column.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: text
    integer :: j
    call w%buf%append(text)
    j = scan(text, achar(10)//achar(13), back=.true.)
    if (j > 0) then
        w%col = utf8_width(text(j + 1:))
    else
        w%col = w%col + utf8_width(text)
    end if
end subroutine put

subroutine emit_range(w, src, tokens, first, last)
    !! Append tokens unchanged.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: first, last
    integer :: j
    do j = first, last
        call put(w, token_text(src, tokens%tok(j)))
    end do
end subroutine emit_range

subroutine emit_blank(w, src, tokens, s)
    !! A blank line: only its line break (and a byte order mark) is kept.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_t), intent(in) :: s
    integer :: j
    do j = s%first, s%last
        if (tokens%tok(j)%kind == TK_NEWLINE .or. tokens%tok(j)%kind == TK_BOM) &
            call put(w, token_text(src, tokens%tok(j)))
    end do
end subroutine emit_blank

subroutine emit_comment_line(w, src, tokens, s, ind, opts)
    !! A comment line, re-indented to `ind`. A comment that lines up with the comment on the line
    !! above (a continued trailing comment) stays lined up with it. Comment lines directly after a
    !! statement that are indented more than it keep their extra indentation. With `keep_column1_comments`, a
    !! comment that starts in column 1 stays there.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_t), intent(in) :: s
    integer, intent(in) :: ind
    type(format_options_t), intent(in) :: opts
    integer :: j, col

    if (.not. opts%indent_comments) then
        call emit_range(w, src, tokens, s%first, s%last)
        w%comment_in = -1
        return
    end if
    do j = s%first, s%last
        select case (tokens%tok(j)%kind)
        case (TK_BOM, TK_NEWLINE)
            call put(w, token_text(src, tokens%tok(j)))
        case (TK_COMMENT)
            if (opts%keep_column1_comments .and. input_column(src, tokens%tok(j)) == 0) then
                col = 0                         ! a comment in column 1 stays there
                w%comment_in = -1
            else
                col = ind
                ! comments directly after a statement and indented deeper than it (such as FORD
                ! `!!` documentation) keep their extra indentation
                if (w%stmt_in >= 0) col = max(ind, w%stmt_out + input_column(src, tokens%tok(j)) - w%stmt_in)
                col = comment_column(w, src, tokens, j, col)
            end if
            call put(w, repeat(' ', col))
            call put(w, trim_blanks(token_text(src, tokens%tok(j))))
        end select
    end do
end subroutine emit_comment_line

integer function comment_column(w, src, tokens, j, default) result(col)
    !! Output column for the comment-only line with comment token `j`: lined up with the comment on
    !! the line above if it was lined up with it in the input, otherwise `default`. Records the
    !! comment for the next line.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: j, default
    integer :: in_col
    in_col = input_column(src, tokens%tok(j))
    col = default
    if (w%comment_in >= 0 .and. in_col == w%comment_in) col = w%comment_out
    col = fitting_column(w, src, tokens, j, j, [col, default, in_col], always_original=.true.)
    w%comment_in = in_col
    w%comment_out = col
end function comment_column

pure integer function input_column(src, t) result(col)
    !! Column of token `t` in the input (characters before it on its line).
    character(len=*), intent(in) :: src
    type(token_t), intent(in) :: t
    integer :: j
    j = scan(src(1:t%first - 1), achar(10)//achar(13), back=.true.)
    col = utf8_width(src(j + 1:t%first - 1))
end function input_column

subroutine emit_statement(w, src, tokens, s, labeled, ind, opts)
    !! A code statement, with its continuation lines and the comment, blank and preprocessor lines
    !! between them. The first line is indented to `ind`; a statement label stays at the start of
    !! the line.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_t), intent(in) :: s
    logical, intent(in) :: labeled
    integer, intent(in) :: ind
    type(format_options_t), intent(in) :: opts
    integer :: j, lend, f
    logical :: first_line, mid_line

    w%nbrackets = 0
    w%anchor_col = -1
    w%pending_anchor = 0
    w%pending_bracket = .false.
    w%separated = .false.
    w%declaration = .false.
    w%prev = ''
    ! a statement after `;` starts in the middle of a line
    mid_line = .false.
    if (s%first > 1) mid_line = tokens%tok(s%first - 1)%kind /= TK_NEWLINE .and. tokens%tok(s%first - 1)%kind /= TK_BOM
    first_line = .true.
    j = s%first
    do while (j <= s%last)
        lend = j
        do while (lend < s%last)
            if (tokens%tok(lend)%kind == TK_NEWLINE) exit
            lend = lend + 1
        end do
        if (tokens%tok(j)%kind == TK_BOM) then
            call put(w, token_text(src, tokens%tok(j)))
            j = j + 1
        end if
        f = first_non_blank(tokens, j, lend)
        w%line_comment = .false.

        if (f > lend) then
            continue                                            ! nothing left on the line
        else if (tokens%tok(f)%kind == TK_NEWLINE) then
            call put(w, token_text(src, tokens%tok(f)))         ! blank line
        else if (tokens%tok(f)%kind == TK_PREPROC) then
            call emit_range(w, src, tokens, j, lend)
        else if (tokens%tok(f)%kind == TK_COMMENT) then
            call put(w, repeat(' ', comment_column(w, src, tokens, f, ind + opts%continuation_indent)))
            call emit_body(w, src, tokens, f, lend)
            w%line_comment = .true.
        else if (first_line) then
            if (mid_line) then
                call emit_body(w, src, tokens, j, lend)
            else if (labeled .and. tokens%tok(f)%kind == TK_INT) then
                call put(w, token_text(src, tokens%tok(f)))
                call put(w, repeat(' ', max(1, ind - w%col)))
                call start_statement(first_non_blank(tokens, f + 1, lend))   ! the code after the label
                call emit_body(w, src, tokens, first_non_blank(tokens, f + 1, lend), lend)
            else
                call put(w, repeat(' ', fitting_column(w, src, tokens, f, lend, &
                                                       [ind, input_column(src, tokens%tok(f))])))
                call start_statement(f)
                call emit_body(w, src, tokens, f, lend)
            end if
            first_line = .false.
        else
            call emit_continuation(w, src, tokens, f, lend, continuation_target(w, ind, opts), &
                                   ind + opts%continuation_indent)
        end if
        if (.not. w%line_comment) w%comment_in = -1
        w%pending_anchor = 0
        w%pending_bracket = .false.
        j = lend + 1
    end do
contains

    subroutine start_statement(f)
        !! Record where the statement starting with token `f` begins, for comment lines after it.
        integer, intent(in) :: f
        w%stmt_in = -1
        if (f > s%last) return
        w%stmt_in = input_column(src, tokens%tok(f))
        w%stmt_out = w%col
        w%generic = token_lower(src, tokens%tok(f)) == 'generic'
    end subroutine start_statement

end subroutine emit_statement

pure integer function continuation_target(w, ind, opts) result(target)
    !! Column for the code of a continuation line: the first item after the innermost open
    !! bracket; outside brackets, the first item after an assignment, a `use` statement's `only:`
    !! or a declaration's `::`; otherwise a fixed indent.
    type(writer_t), intent(in) :: w
    integer, intent(in) :: ind
    type(format_options_t), intent(in) :: opts
    integer :: a
    target = ind + opts%continuation_indent
    if (opts%continuation /= CONT_ALIGN) return
    if (w%nbrackets > 0) then
        if (w%bracket_first(w%nbrackets) >= 0) target = w%bracket_first(w%nbrackets)
        return
    end if
    do a = 1, size(w%anchor_col)
        if (w%anchor_col(a) >= 0) then
            target = w%anchor_col(a)
            return
        end if
    end do
end function continuation_target

subroutine emit_continuation(w, src, tokens, f, lend, aligned, indented)
    !! A continuation line whose first non-blank token is `f`, with its code starting at the column
    !! `aligned`, or at `indented` or its original column if that would make the line too long.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: f, lend, aligned, indented
    integer :: g, target, orig

    if (tokens%tok(f)%kind /= TK_CONT) then
        target = fitting_column(w, src, tokens, f, lend, [aligned, indented, input_column(src, tokens%tok(f))])
        call put(w, repeat(' ', target))
        call emit_body(w, src, tokens, f, lend)
        return
    end if
    ! a leading `&`, placed so that the code after it starts at the target column
    orig = input_column(src, tokens%tok(f))
    g = f + 1
    if (g <= lend) then
        if (is_code_kind(tokens%tok(g)%kind)) then
            ! `&code`: keep them together
            target = fitting_column(w, src, tokens, f, lend, [aligned - 1, indented - 1, orig], g, 1)
            call put(w, repeat(' ', target)//'&')
            call emit_body(w, src, tokens, g, lend)
            return
        end if
    end if
    g = first_non_blank(tokens, g, lend)
    target = fitting_column(w, src, tokens, f, lend, [aligned - 2, indented - 2, orig], g, 2)
    call put(w, repeat(' ', target)//'&')
    if (g <= lend) then
        if (tokens%tok(g)%kind /= TK_NEWLINE) call put(w, ' ')
    end if
    call emit_body(w, src, tokens, g, lend)
end subroutine emit_continuation

subroutine emit_body(w, src, tokens, first, lend)
    !! Tokens `first:lend` of a line (see [[line_pieces]]), while tracking open brackets and the
    !! anchors for aligning continuation lines.
    type(writer_t), intent(inout) :: w
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: first, lend
    type(piece_t), allocatable :: pieces(:)
    integer :: k, t
    character(len=:), allocatable :: text

    call line_pieces(w, src, tokens, first, lend, pieces)
    do k = 1, size(pieces)
        text = pieces(k)%text
        t = pieces(k)%tok
        if (t == 0) then
            call put(w, text)
            cycle
        end if
        if (tokens%tok(t)%kind == TK_COMMENT) then
            if (.not. w%line_comment) then           ! a trailing comment: later lines may line up with it
                w%comment_in = input_column(src, tokens%tok(t))
                w%comment_out = w%col
                w%line_comment = .true.
            end if
        end if
        if (.not. is_code_kind(tokens%tok(t)%kind)) then
            call put(w, text)
            cycle
        end if
        if (w%pending_bracket) w%bracket_first(w%nbrackets) = w%col
        if (w%pending_anchor > 0) w%anchor_col(w%pending_anchor) = w%col
        w%pending_bracket = .false.
        w%pending_anchor = 0
        call put(w, text)
        if (tokens%tok(t)%kind == TK_OP) then
            select case (text)
            case ('(', '[')
                if (w%nbrackets < MAX_BRACKETS) then
                    w%nbrackets = w%nbrackets + 1
                    w%bracket_first(w%nbrackets) = -1
                    w%pending_bracket = .true.
                end if
            case (')', ']')
                if (w%nbrackets > 0) w%nbrackets = w%nbrackets - 1
            case default
                if (w%nbrackets == 0) call top_level_operator(text)
            end select
        end if
        w%prev = token_lower(src, tokens%tok(t))
    end do

contains

    subroutine top_level_operator(op)
        !! Record alignment anchors at a top-level operator.
        character(len=*), intent(in) :: op
        select case (op)
        case ('=', '=>')
            ! an assignment, unless a `,` or `:` came first (`use m, only: a => b`, `integer :: n = 1`),
            ! or the bindings of `generic :: name => a, b`
            if (.not. w%separated) then
                call anchor(ANCHOR_ASSIGN)
            else if (op == '=>' .and. w%generic .and. w%declaration) then
                call anchor(ANCHOR_ASSIGN)
            end if
        case ('::')
            call anchor(ANCHOR_DECL)
            w%separated = .true.
            w%declaration = .true.
        case (':')
            if (w%prev == 'only') call anchor(ANCHOR_ONLY)
            w%separated = .true.
        case (',')
            w%separated = .true.
        end select
    end subroutine top_level_operator

    subroutine anchor(a)
        integer, intent(in) :: a
        if (w%anchor_col(a) < 0) w%pending_anchor = a
    end subroutine anchor
end subroutine emit_body

pure integer function first_non_blank(tokens, first, last) result(f)
    !! First token in `first:last` that is not a blank, or `last + 1`.
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: first, last
    do f = first, last
        if (tokens%tok(f)%kind /= TK_SPACE) return
    end do
    f = last + 1
end function first_non_blank

pure function to_case(text, upper) result(t)
    !! `text` in ASCII upper or lower case.
    character(len=*), intent(in) :: text
    logical, intent(in) :: upper
    character(len=len(text)) :: t
    integer :: k, c
    t = text
    do k = 1, len(t)
        c = iachar(t(k:k))
        if (upper .and. c >= iachar('a') .and. c <= iachar('z')) t(k:k) = achar(c - 32)
        if (.not. upper .and. c >= iachar('A') .and. c <= iachar('Z')) t(k:k) = achar(c + 32)
    end do
end function to_case

pure function trim_blanks(text) result(t)
    !! `text` without trailing blanks and tabs.
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: t
    integer :: n
    n = len(text)
    do while (n > 0)
        if (text(n:n) /= ' ' .and. text(n:n) /= achar(9)) exit
        n = n - 1
    end do
    t = text(1:n)
end function trim_blanks

end module ffmt_layout
