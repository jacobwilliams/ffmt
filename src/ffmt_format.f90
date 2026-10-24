module ffmt_format
!! Formatting a whole source text: the main library entry point.
use ffmt_tokens
use ffmt_lexer, only: tokenize
use ffmt_statements, only: statement_t, statement_view_t, statement_view, SK_CODE, SK_PREPROC
use ffmt_source, only: source_t, parse_source, statement_views
use ffmt_structure, only: analyze_structure, diagnostic_t, structure_t
use ffmt_layout, only: layout
use ffmt_whitespace, only: compute_gaps
use ffmt_case, only: compute_case
use ffmt_relational, only: convert_relational, canonical_relational
use ffmt_linebreak, only: split_long_lines, long_code_lines
use ffmt_options, only: format_options_t
use ffmt_use_sort, only: sort_use, is_use_statement, only_items, sorted_order
use ffmt_strings, only: to_s, string_t, string_builder_t, lower_char, lowercase
implicit none
private

public :: format_text, same_code, longest_code
public :: diagnostic_t, format_options_t

integer, parameter, public :: FMT_OK = 0              !! formatted
integer, parameter, public :: FMT_ERR_INTERNAL = 1    !! formatting would have changed the code; the text is returned unchanged

contains

subroutine format_text(src, opts, out, stat, errmsg, diags)
    !! Format `src`. On success `out` is the formatted text and `stat` is `FMT_OK`.
    !!
    !! As a safety check, the code tokens of the result are compared with those of the input. If they
    !! differ, which would be a bug in the formatter, `out` is the unchanged input and `stat` is
    !! `FMT_ERR_INTERNAL`.
    character(len=*), intent(in) :: src                    !! source text to format
    type(format_options_t), intent(in) :: opts             !! formatting options
    character(len=:), allocatable, intent(out) :: out      !! the formatted text
    integer, intent(out) :: stat                           !! `FMT_OK` or `FMT_ERR_INTERNAL`
    character(len=:), allocatable, intent(out) :: errmsg   !! error message; unallocated if `stat` is `FMT_OK`
    type(diagnostic_t), allocatable, intent(out), optional :: diags(:)   !! problems found while formatting

    integer, parameter :: MAX_PASSES = 50   ! each pass splits each long line once
    type(diagnostic_t), allocatable :: d(:), unused(:)
    type(token_list_t) :: tokens
    integer, allocatable :: long(:), changed(:, :)
    character(len=:), allocatable :: prepared, split
    integer :: k, nsplit

    ! a sorted `only:` list is written on one line for splitting, or broken to fit if splitting is off
    prepared = convert_relational(sort_use(src, opts%sort_use, opts%sort_use_only, &
                                           merge(0, opts%line_length, opts%split_lines)), opts%relational)
    call format_pass(prepared, opts, out, d, tokens)
    ! no code longer than the limit, as most files: nothing to split, and no need to look for
    ! code lines that are too long
    if (opts%split_lines .and. opts%line_length > 0 .and. longest_code(out) > opts%line_length) then
        ! split long lines and format again, until every line fits or nothing more can be split; only
        ! the statements that were split need laying out again, unless one of them depends on the
        ! lines around it
        do k = 1, MAX_PASSES
            split = split_long_lines(out, opts%line_length, opts%continuation_indent, nsplit, changed)
            if (nsplit == 0) exit
            if (allocated(changed) .and. opts%comment_spacing < 0) then
                out = relayout(split, changed, opts)
            else
                call format_pass(split, opts, out, unused)
            end if
            if (longest_code(out) <= opts%line_length) exit
        end do
        if (longest_code(out) > opts%line_length) then
            long = long_code_lines(out, opts%line_length)
            do k = 1, size(long)
                d = [d, diagnostic_t(long(k), 'line '//to_s(long(k))//' of the formatted file is longer than '// &
                                     to_s(opts%line_length)//' characters and cannot be split')]
            end do
        end if
    end if
    if (opts%comment_spacing >= 0) then
        ! comments are kept where they line up in the input; once the code is formatted, some of
        ! them may no longer line up, so decide again on the result (which is then stable)
        split = out
        call format_pass(split, opts, out, unused)
    end if
    d = [limit_diagnostics(src), d]
    if (present(diags)) diags = d

    stat = FMT_OK
    ! the tokens of the first pass are those of `src` if sorting and relational spelling changed nothing
    if (len(prepared) /= len(src) .or. prepared /= src) call tokenize(src, tokens)
    if (.not. same_code(src, out, opts%sort_use .or. opts%sort_use_only, tokens)) then
        stat = FMT_ERR_INTERNAL
        errmsg = 'internal error: formatting would change the code, so the file was left unchanged'
        out = src
    end if
end subroutine format_text

subroutine format_pass(src, opts, out, diags, src_tokens)
    !! One pass of the formatter: structure, whitespace, case and layout.
    character(len=*), intent(in) :: src                         !! source text
    type(format_options_t), intent(in) :: opts                  !! formatting options
    character(len=:), allocatable, intent(out) :: out           !! the formatted text
    type(diagnostic_t), allocatable, intent(out) :: diags(:)    !! problems found in this pass
    type(token_list_t), intent(out), optional :: src_tokens     !! the tokens of `src`

    type(source_t) :: source
    type(statement_view_t), allocatable :: views(:)
    type(structure_t) :: st
    integer, allocatable :: long_lines(:)
    integer :: k

    call parse_source(src, source)
    views = statement_views(source)
    call analyze_structure(source, views, st, diags)
    out = layout(source, st, compute_gaps(source, views, opts%spaces, opts%comment_spacing), &
                 compute_case(source, views, opts%letter_case), opts, long_lines)
    do k = 1, size(long_lines)
        diags = [diags, diagnostic_t(long_lines(k), 'indentation kept: the line would be longer than '// &
                                     to_s(opts%line_length)//' characters')]
    end do
    if (present(src_tokens)) then
        call move_alloc(source%tokens%tok, src_tokens%tok)
        src_tokens%n = source%tokens%n
    end if
end subroutine format_pass

function limit_diagnostics(src) result(diags)
    !! Warnings for input beyond the limits of the Fortran 2023 standard (6.3.2.1): lines longer than
    !! 10,000 characters, and statements of more than 1,000,000 characters (counted without comments
    !! and line breaks).
    character(len=*), intent(in) :: src      !! source text
    type(diagnostic_t), allocatable :: diags(:)
    integer, parameter :: MAX_LINE = 10000, MAX_STATEMENT = 1000000
    type(source_t) :: source
    integer :: k, j, n, line, width, c

    allocate (diags(0))
    if (longest_line(src) > MAX_LINE) then
        line = 1
        width = 0
        do k = 1, len(src)
            c = iand(ichar(src(k:k)), 255)
            if (c == 10) then
                line = line + 1
                width = 0
            else if (c < 128 .or. c >= 192) then
                width = width + 1
                if (width == MAX_LINE + 1) diags = [diags, diagnostic_t(line, 'line '//to_s(line)// &
                                                                        ' is longer than '//to_s(MAX_LINE)//' characters, '// &
                                                                        'the limit of the Fortran standard')]
            end if
        end do
    end if
    if (len(src) <= MAX_STATEMENT) return
    call parse_source(src, source)
    do k = 1, source%stmts%n
        if (source%stmts%s(k)%kind /= SK_CODE) cycle
        n = 0
        do j = source%stmts%s(k)%first, source%stmts%s(k)%last
            associate (t => source%tokens%tok(j))
                if (t%kind /= TK_COMMENT .and. t%kind /= TK_NEWLINE) n = n + t%last - t%first + 1
            end associate
        end do
        if (n > MAX_STATEMENT) then
            line = source%tokens%tok(source%stmts%s(k)%first)%line
            diags = [diags, diagnostic_t(line, 'the statement on line '//to_s(line)//' has more than '// &
                                         to_s(MAX_STATEMENT)//' characters, the limit of the Fortran standard')]
        end if
    end do
end function limit_diagnostics

function relayout(text, changed, opts) result(out)
    !! `text` with the statements at `changed` (byte ranges, in order) laid out again, each on its own
    !! at the column where it starts. `text` is formatted apart from those statements, and they do
    !! not depend on the lines around them (see [[stands_alone]]), so this gives what formatting the
    !! whole text again would, in a fraction of the time.
    character(len=*), intent(in) :: text                 !! the text, after lines were split
    integer, intent(in) :: changed(:, :)                 !! byte ranges of the statements to lay out
    type(format_options_t), intent(in) :: opts           !! the formatting options
    character(len=:), allocatable :: out
    type(format_options_t) :: alone
    type(diagnostic_t), allocatable :: unused(:)
    type(string_builder_t) :: buf
    character(len=:), allocatable :: piece
    integer :: j, next

    alone = opts
    next = 1
    do j = 1, size(changed, 2)
        associate (a => changed(1, j), b => changed(2, j))
            call buf%append(text(next:a - 1))
            alone%base_indent = verify(text(a:b)//'x', ' ') - 1    ! its indentation
            call format_pass(text(a:b), alone, piece, unused)
            call buf%append(piece)
            next = b + 1
        end associate
    end do
    call buf%append(text(next:))
    out = buf%get()
end function relayout

pure integer function longest_code(text) result(width)
    !! An upper bound of the width of the longest code line of `text`: the characters of each line
    !! before a comment, without the blanks before it, as [[long_code_lines]] counts them.
    !! Preprocessor lines do not count. Strings are followed across continuation lines, so that a
    !! `!` inside one does not end the code, but the scan does not tokenize, so it may count more
    !! than a token-based count would; never less.
    character(len=*), intent(in) :: text   !! the formatted text
    integer :: k, c, w, code_w, last_nonblank
    character :: quote
    logical :: line_start, skip, in_comment

    width = 0
    w = 0
    code_w = 0
    quote = ' '
    line_start = .true.
    skip = .false.
    in_comment = .false.
    last_nonblank = 0
    do k = 1, len(text)
        c = iand(ichar(text(k:k)), 255)
        if (c == 13 .and. k < len(text)) then
            if (text(k + 1:k + 1) == achar(10)) cycle      ! CRLF: the line ends at the LF
        end if
        if (c == 10 .or. c == 13) then
            width = max(width, code_w)
            ! a string continued on the next line ends this one with `&`
            if (quote /= ' ' .and. .not. (last_nonblank > 0 .and. text(max(1, last_nonblank):max(1, last_nonblank)) == '&')) &
                quote = ' '
            w = 0
            code_w = 0
            line_start = .true.
            skip = .false.
            in_comment = .false.
            last_nonblank = 0
            cycle
        end if
        if (skip .or. in_comment) cycle
        if (c >= 128 .and. c < 192) cycle                  ! a UTF-8 continuation byte
        w = w + 1
        if (text(k:k) == ' ' .or. text(k:k) == achar(9)) cycle
        if (line_start) then
            line_start = .false.
            if (quote == ' ' .and. (text(k:k) == '#' .or. (index('$@', text(k:k)) > 0 .and. &
                                                           text(min(k + 1, len(text)):min(k + 1, len(text))) == ':'))) then
                skip = .true.                              ! a preprocessor line
                cycle
            end if
        end if
        if (quote /= ' ') then
            if (text(k:k) == quote) quote = ' '            ! a doubled quote reopens at once
        else if (text(k:k) == '"' .or. text(k:k) == "'") then
            quote = text(k:k)
        else if (text(k:k) == '!') then
            in_comment = .true.
            cycle
        end if
        code_w = w
        last_nonblank = k
    end do
    width = max(width, code_w)
end function longest_code

pure integer function longest_line(text) result(width)
    !! Width of the longest line of `text`, in characters (as [[utf8_width]] counts them).
    character(len=*), intent(in) :: text   !! text whose lines end with LF
    integer :: k, c, w
    width = 0
    w = 0
    do k = 1, len(text)
        c = iand(ichar(text(k:k)), 255)
        if (c == 10) then
            w = 0
        else if (c < 128 .or. c >= 192) then
            w = w + 1
            width = max(width, w)
        end if
    end do
end function longest_line

logical function same_code(a, b, reordered_use, a_tokens)
    !! True if `a` and `b` have the same code tokens, in the same order, with the same text and the
    !! same joins across line breaks (blanks, comments and line layout may differ). Names and dot
    !! operators are compared ignoring case, as Fortran does, and the two spellings of a relational
    !! operator (`<` and `.lt.`) are the same.
    !!
    !! With `reordered_use`, `use` statements may also be in a different order within a run of them
    !! (not across other statements or preprocessor lines), and so may the items of an `only:` list.
    character(len=*), intent(in) :: a                     !! first source text
    character(len=*), intent(in) :: b                     !! second source text
    logical, intent(in), optional :: reordered_use        !! allow reordered `use` statements; default false
    type(token_list_t), intent(in), optional :: a_tokens    !! the tokens of `a`, if already known
    type(token_list_t) :: ta, tb
    integer :: i, j

    if (present(reordered_use)) then
        if (reordered_use) then
            same_code = code_signature(a) == code_signature(b)
            return
        end if
    end if
    ! token by token, without building signatures
    if (present(a_tokens)) then
        ta = a_tokens
    else
        call tokenize(a, ta)
    end if
    call tokenize(b, tb)
    same_code = .false.
    i = 0
    j = 0
    do
        i = next_code(ta, i)
        j = next_code(tb, j)
        if (i > ta%n .or. j > tb%n) exit
        if (.not. same_token(a, ta%tok(i), b, tb%tok(j))) return
    end do
    same_code = i > ta%n .and. j > tb%n

contains

    pure integer function next_code(tokens, k) result(next)
        !! The index of the first code token after token `k`, or `tokens%n + 1`.
        type(token_list_t), intent(in) :: tokens   !! the token list
        integer, intent(in) :: k                   !! index into `tokens`, or 0 to start at the beginning
        next = k + 1
        do while (next <= tokens%n)
            if (is_code_kind(tokens%tok(next)%kind)) return
            next = next + 1
        end do
    end function next_code

end function same_code

pure logical function same_token(sa, s, sb, t)
    !! True if code token `s` of `sa` matches code token `t` of `sb`, as their keys in
    !! [[code_signature]] would: same kind (operators all alike), same glue, and the same text,
    !! ignoring case where Fortran does (see [[folded_length]]), with relational operators in their
    !! symbol spelling.
    character(len=*), intent(in) :: sa   !! source text of `s`
    character(len=*), intent(in) :: sb   !! source text of `t`
    type(token_t), intent(in) :: s       !! a code token of `sa`
    type(token_t), intent(in) :: t       !! a code token of `sb`
    integer :: ks, kt, k, nfold
    character(len=:), allocatable :: ca, cb

    ks = s%kind
    kt = t%kind
    if (ks == TK_DOTOP) ks = TK_OP
    if (kt == TK_DOTOP) kt = TK_OP
    same_token = .false.
    if (ks /= kt .or. (s%glued .neqv. t%glued)) return
    if (s%last - s%first == t%last - t%first) then
        nfold = folded_length(sa(s%first:s%last), s%kind)
        same_token = .true.
        do k = 0, s%last - s%first
            if (k < nfold) then
                if (lower_char(sa(s%first + k:s%first + k)) == lower_char(sb(t%first + k:t%first + k))) cycle
            else
                if (sa(s%first + k:s%first + k) == sb(t%first + k:t%first + k)) cycle
            end if
            same_token = .false.
            exit
        end do
        if (same_token) return
    end if
    ! `.lt.` and `<`: rare, so the allocating comparison is fine here
    if (ks == TK_OP) then
        ca = canonical_relational(token_lower(sa, s))
        cb = canonical_relational(token_lower(sb, t))
        same_token = len(ca) == len(cb) .and. ca == cb
    end if
end function same_token

pure integer function folded_length(text, kind) result(n)
    !! How much of a token's text, from its start, Fortran reads ignoring case: all of a name, an
    !! operator or a number (exponent letters and kinds), the kind prefix of a string, and none of
    !! anything else.
    character(len=*), intent(in) :: text   !! the token's text
    integer, intent(in) :: kind            !! its kind (`TK_*`)
    select case (kind)
    case (TK_NAME, TK_OP, TK_DOTOP, TK_INT, TK_REAL)
        n = len(text)
    case (TK_STRING)
        n = max(0, scan(text, '"'//"'") - 1)
    case default
        n = 0
    end select
end function folded_length

function code_signature(src) result(sig)
    !! The code tokens of `src` as one string that [[same_code]] compares when `use` statements may
    !! be reordered: for each token its kind (operators all alike), whether it is glued to the
    !! previous token, and its text as `key` gives it. The signatures of `use` statements in a run
    !! are sorted, and so are those of the items of each `only:` list.
    character(len=*), intent(in) :: src   !! source text
    character(len=:), allocatable :: sig

    type(source_t) :: source
    type(statement_view_t) :: v
    type(string_builder_t) :: buf
    type(string_t), allocatable :: run(:)
    type(string_t) :: item
    integer :: k, j

    call parse_source(src, source)
    allocate (run(0))
    associate (tokens => source%tokens, stmts => source%stmts)
        do k = 1, stmts%n
            if (stmts%s(k)%kind == SK_PREPROC) call flush_run()
            if (stmts%s(k)%kind /= SK_CODE) cycle
            v = statement_view(src, tokens, stmts%s(k))
            if (is_use_statement(v)) then
                ! through a variable: a gfortran 15 internal compiler error otherwise
                item%s = use_signature(stmts%s(k), v)
                run = [run, item]
            else
                call flush_run()
                do j = stmts%s(k)%first, stmts%s(k)%last
                    if (is_code_kind(tokens%tok(j)%kind)) call buf%append(key(tokens%tok(j)))
                end do
            end if
        end do
    end associate
    call flush_run()
    sig = buf%get()

contains

    subroutine flush_run()
        !! Append the signatures of the pending run of `use` statements, sorted, and empty the run.
        integer, allocatable :: order(:)
        integer :: p
        order = sorted_order(run)
        do p = 1, size(order)
            call buf%append(run(order(p))%s)
        end do
        deallocate (run)
        allocate (run(0))
    end subroutine flush_run

    function use_signature(stmt, v) result(s)
        !! Signature of a `use` statement, with the items of its `only:` list sorted. Any `;` that
        !! ends it comes last.
        type(statement_t), intent(in) :: stmt          !! the `use` statement
        type(statement_view_t), intent(in) :: v        !! its view
        character(len=:), allocatable :: s
        integer, allocatable :: starts(:), ends(:), order(:)
        type(string_t), allocatable :: items(:)
        integer :: p, q, first

        starts = only_items(v)
        first = v%n + 1
        if (size(starts) > 0) first = starts(1)
        s = ''
        do p = 1, first - 1
            s = s//key(source%tokens%tok(v%code(p)))
        end do
        if (size(starts) > 0) then
            ends = [starts(2:) - 2, v%n]
            allocate (items(size(starts)))
            do p = 1, size(starts)
                items(p)%s = ''
                do q = starts(p), ends(p)
                    items(p)%s = items(p)%s//key(source%tokens%tok(v%code(q)))
                end do
            end do
            order = sorted_order(items)
            do p = 1, size(order)
                s = s//items(order(p))%s
                if (p < size(order)) s = s//key(source%tokens%tok(v%code(starts(2) - 1)))   ! the comma
            end do
        end if
        do p = stmt%first, stmt%last
            if (source%tokens%tok(p)%kind == TK_OP) then
                if (token_text(src, source%tokens%tok(p)) == ';') s = s//key(source%tokens%tok(p))
            end if
        end do
    end function use_signature

    function key(t) result(k)
        !! What must match: the kind, the glue, and the text, ignoring case for names and operators,
        !! with relational operators in their symbol spelling. The length is included, since `==`
        !! pads with blanks.
        type(token_t), intent(in) :: t   !! a code token of `src`
        character(len=:), allocatable :: k
        character :: kind, glue
        select case (t%kind)
        case (TK_DOTOP, TK_OP)
            k = canonical_relational(token_lower(src, t))
        case default
            k = token_text(src, t)
            associate (n => folded_length(k, t%kind))
                k(1:n) = lowercase(k(1:n))
            end associate
        end select
        kind = achar(64 + t%kind)
        if (t%kind == TK_DOTOP) kind = achar(64 + TK_OP)
        glue = merge('g', ' ', t%glued)
        k = kind//glue//to_s(len(k))//':'//k
    end function key

end function code_signature

end module ffmt_format
