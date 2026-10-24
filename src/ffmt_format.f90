module ffmt_format
!! Formatting a whole source text: the main library entry point.
use ffmt_tokens
use ffmt_lexer, only: tokenize
use ffmt_statements, only: statement_t, statement_list_t, split_statements, code_tokens, SK_CODE, SK_PREPROC
use ffmt_constructs, only: stmt_info_t
use ffmt_structure, only: analyze_structure, diagnostic_t
use ffmt_layout, only: layout
use ffmt_whitespace, only: compute_gaps
use ffmt_case, only: compute_case
use ffmt_relational, only: convert_relational, canonical_relational
use ffmt_linebreak, only: split_long_lines, long_code_lines
use ffmt_options, only: format_options_t
use ffmt_use_sort, only: sort_use, is_use_statement, only_items, sorted_order
use ffmt_strings, only: to_s, string_t, string_builder_t
implicit none
private

public :: format_text, same_code
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
    character(len=*), intent(in) :: src
    type(format_options_t), intent(in) :: opts
    character(len=:), allocatable, intent(out) :: out
    integer, intent(out) :: stat
    character(len=:), allocatable, intent(out) :: errmsg
    type(diagnostic_t), allocatable, intent(out), optional :: diags(:)   !! problems found while formatting

    integer, parameter :: MAX_PASSES = 50   ! each pass splits each long line once
    type(diagnostic_t), allocatable :: d(:), unused(:)
    type(token_list_t) :: tokens
    integer, allocatable :: long(:)
    character(len=:), allocatable :: prepared, split
    integer :: k, nsplit

    ! a sorted `only:` list is written on one line for splitting, or broken to fit if splitting is off
    prepared = convert_relational(sort_use(src, opts%sort_use, opts%sort_use_only, &
                                           merge(0, opts%line_length, opts%split_lines)), opts%relational)
    call format_pass(prepared, opts, out, d, tokens)
    ! no line longer than the limit, as most files: nothing to split, and no need to look for
    ! code lines that are too long
    if (opts%split_lines .and. opts%line_length > 0 .and. longest_line(out) > opts%line_length) then
        ! split long lines and format again, until every line fits or nothing more can be split
        do k = 1, MAX_PASSES
            split = split_long_lines(out, opts%line_length, opts%continuation_indent, nsplit)
            if (nsplit == 0) exit
            call format_pass(split, opts, out, unused)
            if (longest_line(out) <= opts%line_length) exit
        end do
        if (longest_line(out) > opts%line_length) then
            long = long_code_lines(out, opts%line_length)
            do k = 1, size(long)
                d = [d, diagnostic_t(long(k), 'line '//to_s(long(k))//' of the formatted file is longer than '// &
                                     to_s(opts%line_length)//' characters and cannot be split')]
            end do
        end if
    end if
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
    character(len=*), intent(in) :: src
    type(format_options_t), intent(in) :: opts
    character(len=:), allocatable, intent(out) :: out
    type(diagnostic_t), allocatable, intent(out) :: diags(:)
    type(token_list_t), intent(out), optional :: src_tokens     !! the tokens of `src`

    type(token_list_t) :: tokens
    type(statement_list_t) :: stmts
    type(stmt_info_t), allocatable :: info(:)
    integer, allocatable :: depth(:), units(:), procedures(:), long_lines(:)
    integer :: k

    call tokenize(src, tokens)
    call split_statements(src, tokens, stmts)
    call analyze_structure(src, tokens, stmts, info, depth, diags, units, procedures)
    out = layout(src, tokens, stmts, info, depth, units, procedures, compute_gaps(src, tokens, stmts, opts%spaces), &
                 compute_case(src, tokens, stmts, opts%letter_case), opts, long_lines)
    do k = 1, size(long_lines)
        diags = [diags, diagnostic_t(long_lines(k), 'indentation kept: the line would be longer than '// &
                                     to_s(opts%line_length)//' characters')]
    end do
    if (present(src_tokens)) then
        call move_alloc(tokens%tok, src_tokens%tok)
        src_tokens%n = tokens%n
    end if
end subroutine format_pass

pure integer function longest_line(text) result(width)
    !! Width of the longest line of `text`, in characters (as [[utf8_width]] counts them).
    character(len=*), intent(in) :: text
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
    character(len=*), intent(in) :: a, b
    logical, intent(in), optional :: reordered_use
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
        type(token_list_t), intent(in) :: tokens
        integer, intent(in) :: k
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
    !! ignoring case for names and operators, with relational operators in their symbol spelling.
    character(len=*), intent(in) :: sa, sb
    type(token_t), intent(in) :: s, t
    integer :: ks, kt, k
    character(len=:), allocatable :: ca, cb

    ks = s%kind
    kt = t%kind
    if (ks == TK_DOTOP) ks = TK_OP
    if (kt == TK_DOTOP) kt = TK_OP
    same_token = .false.
    if (ks /= kt .or. (s%glued .neqv. t%glued)) return
    if (ks /= TK_NAME .and. ks /= TK_OP) then
        same_token = s%last - s%first == t%last - t%first .and. sa(s%first:s%last) == sb(t%first:t%last)
        return
    end if
    if (s%last - s%first == t%last - t%first) then
        same_token = .true.
        do k = 0, s%last - s%first
            if (lower(sa(s%first + k:s%first + k)) /= lower(sb(t%first + k:t%first + k))) then
                same_token = .false.
                exit
            end if
        end do
        if (same_token) return
    end if
    ! `.lt.` and `<`: rare, so the allocating comparison is fine here
    if (ks == TK_OP) then
        ca = canonical_relational(token_lower(sa, s))
        cb = canonical_relational(token_lower(sb, t))
        same_token = len(ca) == len(cb) .and. ca == cb
    end if

contains

    pure character function lower(c)
        character, intent(in) :: c
        lower = c
        if (c >= 'A' .and. c <= 'Z') lower = achar(iachar(c) + 32)
    end function lower

end function same_token

function code_signature(src) result(sig)
    !! The code tokens of `src` as one string that [[same_code]] compares when `use` statements may
    !! be reordered: for each token its kind (operators all alike), whether it is glued to the
    !! previous token, and its text as `key` gives it. The signatures of `use` statements in a run
    !! are sorted, and so are those of the items of each `only:` list.
    character(len=*), intent(in) :: src
    character(len=:), allocatable :: sig

    type(token_list_t) :: tokens
    type(statement_list_t) :: stmts
    type(string_builder_t) :: buf
    type(string_t), allocatable :: run(:)
    type(string_t) :: item
    integer, allocatable :: code(:)
    integer :: k, j

    call tokenize(src, tokens)
    call split_statements(src, tokens, stmts)
    allocate (run(0))
    do k = 1, stmts%n
        if (stmts%s(k)%kind == SK_PREPROC) call flush_run()
        if (stmts%s(k)%kind /= SK_CODE) cycle
        code = code_tokens(src, tokens, stmts%s(k))
        if (is_use_statement(src, tokens, code)) then
            ! through a variable: a gfortran 15 internal compiler error otherwise
            item%s = use_signature(stmts%s(k), code)
            run = [run, item]
        else
            call flush_run()
            do j = stmts%s(k)%first, stmts%s(k)%last
                if (is_code_kind(tokens%tok(j)%kind)) call buf%append(key(tokens%tok(j)))
            end do
        end if
    end do
    call flush_run()
    sig = buf%get()

contains

    subroutine flush_run()
        integer, allocatable :: order(:)
        integer :: p
        order = sorted_order(run)
        do p = 1, size(order)
            call buf%append(run(order(p))%s)
        end do
        deallocate (run)
        allocate (run(0))
    end subroutine flush_run

    function use_signature(stmt, code) result(s)
        !! Signature of a `use` statement, with the items of its `only:` list sorted. Any `;` that
        !! ends it comes last.
        type(statement_t), intent(in) :: stmt
        integer, intent(in) :: code(:)
        character(len=:), allocatable :: s
        integer, allocatable :: starts(:), ends(:), order(:)
        type(string_t), allocatable :: items(:)
        integer :: p, q, first

        starts = only_items(src, tokens, code)
        first = size(code) + 1
        if (size(starts) > 0) first = starts(1)
        s = ''
        do p = 1, first - 1
            s = s//key(tokens%tok(code(p)))
        end do
        if (size(starts) > 0) then
            ends = [starts(2:) - 2, size(code)]
            allocate (items(size(starts)))
            do p = 1, size(starts)
                items(p)%s = ''
                do q = starts(p), ends(p)
                    items(p)%s = items(p)%s//key(tokens%tok(code(q)))
                end do
            end do
            order = sorted_order(items)
            do p = 1, size(order)
                s = s//items(order(p))%s
                if (p < size(order)) s = s//key(tokens%tok(code(starts(2) - 1)))   ! the comma
            end do
        end if
        do p = stmt%first, stmt%last
            if (tokens%tok(p)%kind == TK_OP) then
                if (token_text(src, tokens%tok(p)) == ';') s = s//key(tokens%tok(p))
            end if
        end do
    end function use_signature

    function key(t) result(k)
        !! What must match: the kind, the glue, and the text, ignoring case for names and operators,
        !! with relational operators in their symbol spelling. The length is included, since `==`
        !! pads with blanks.
        type(token_t), intent(in) :: t
        character(len=:), allocatable :: k
        character :: kind, glue
        select case (t%kind)
        case (TK_NAME)
            k = token_lower(src, t)
        case (TK_DOTOP, TK_OP)
            k = canonical_relational(token_lower(src, t))
        case default
            k = token_text(src, t)
        end select
        kind = achar(64 + t%kind)
        if (t%kind == TK_DOTOP) kind = achar(64 + TK_OP)
        glue = merge('g', ' ', t%glued)
        k = kind//glue//to_s(len(k))//':'//k
    end function key

end function code_signature

end module ffmt_format
