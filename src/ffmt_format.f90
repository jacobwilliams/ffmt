module ffmt_format
!! Formatting a whole source text: the main library entry point.
use ffmt_tokens
use ffmt_lexer, only: tokenize
use ffmt_statements, only: statement_list_t, split_statements
use ffmt_constructs, only: stmt_info_t
use ffmt_structure, only: analyze_structure, diagnostic_t
use ffmt_layout, only: layout
use ffmt_whitespace, only: compute_gaps
use ffmt_case, only: compute_case
use ffmt_relational, only: convert_relational, canonical_relational
use ffmt_linebreak, only: split_long_lines, long_code_lines
use ffmt_options, only: format_options_t
use ffmt_strings, only: to_s
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
    integer, allocatable :: long(:)
    character(len=:), allocatable :: split
    integer :: k, nsplit

    call format_pass(convert_relational(src, opts%relational), opts, out, d)
    if (opts%split_lines .and. opts%line_length > 0) then
        ! split long lines and format again, until every line fits or nothing more can be split
        do k = 1, MAX_PASSES
            split = split_long_lines(out, opts%line_length, opts%continuation_indent, nsplit)
            if (nsplit == 0) exit
            call format_pass(split, opts, out, unused)
        end do
        long = long_code_lines(out, opts%line_length)
        do k = 1, size(long)
            d = [d, diagnostic_t(long(k), 'line '//to_s(long(k))//' of the formatted file is longer than '// &
                                 to_s(opts%line_length)//' characters and cannot be split')]
        end do
    end if
    if (present(diags)) diags = d

    stat = FMT_OK
    if (.not. same_code(src, out)) then
        stat = FMT_ERR_INTERNAL
        errmsg = 'internal error: formatting would change the code, so the file was left unchanged'
        out = src
    end if
end subroutine format_text

subroutine format_pass(src, opts, out, diags)
    !! One pass of the formatter: structure, whitespace, case and layout.
    character(len=*), intent(in) :: src
    type(format_options_t), intent(in) :: opts
    character(len=:), allocatable, intent(out) :: out
    type(diagnostic_t), allocatable, intent(out) :: diags(:)

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
end subroutine format_pass

logical function same_code(a, b)
    !! True if `a` and `b` have the same code tokens, in the same order, with the same text and the
    !! same joins across line breaks (blanks, comments and line layout may differ). Names and dot
    !! operators are compared ignoring case, as Fortran does, and the two spellings of a relational
    !! operator (`<` and `.lt.`) are the same.
    character(len=*), intent(in) :: a, b
    type(token_list_t) :: ta, tb
    integer :: i, j

    call tokenize(a, ta)
    call tokenize(b, tb)
    same_code = .false.
    i = 0
    j = 0
    do
        i = next_code(ta, i)
        j = next_code(tb, j)
        if (i > ta%n .or. j > tb%n) exit
        if (key(a, ta%tok(i)) /= key(b, tb%tok(j))) return
        if (ta%tok(i)%glued .neqv. tb%tok(j)%glued) return
        if (ta%tok(i)%kind /= tb%tok(j)%kind) then
            if (.not. (is_operator(ta%tok(i)) .and. is_operator(tb%tok(j)))) return
        end if
    end do
    same_code = i > ta%n .and. j > tb%n

contains

    function key(src, t) result(k)
        !! What must match: the text, ignoring case for names and operators, with relational
        !! operators in their symbol spelling. The length is included, since `==` pads with blanks.
        character(len=*), intent(in) :: src
        type(token_t), intent(in) :: t
        character(len=:), allocatable :: k
        select case (t%kind)
        case (TK_NAME)
            k = token_lower(src, t)
        case (TK_DOTOP, TK_OP)
            k = canonical_relational(token_lower(src, t))
        case default
            k = token_text(src, t)
        end select
        k = to_s(len(k))//':'//k
    end function key

    logical function is_operator(t)
        type(token_t), intent(in) :: t
        is_operator = t%kind == TK_OP .or. t%kind == TK_DOTOP
    end function is_operator

end function same_code

pure integer function next_code(tokens, i) result(j)
    !! Index of the next code token after `i`, or `tokens%n + 1`.
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: i
    do j = i + 1, tokens%n
        if (is_code_kind(tokens%tok(j)%kind)) return
    end do
    j = tokens%n + 1
end function next_code

end module ffmt_format
