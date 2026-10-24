module ffmt_statements
!! Groups tokens into statements and standalone lines.
!!
!! A code statement runs from its first token to the line break that ends it, including any
!! continuation lines and the comment, blank and preprocessor lines between them. Statements
!! separated by `;` on one line are split after the `;`. Lines outside statements that hold only
!! blanks, a comment or a preprocessor directive become items of their own. Together the items
!! cover every token exactly once, in order.
use ffmt_tokens
implicit none
private

public :: split_statements, code_tokens

! item kinds
integer, parameter, public :: SK_CODE = 1
integer, parameter, public :: SK_BLANK = 2
integer, parameter, public :: SK_COMMENT = 3
integer, parameter, public :: SK_PREPROC = 4

type, public :: statement_t
    integer :: kind = SK_CODE
    integer :: first = 1    !! first token
    integer :: last = 0     !! last token
end type statement_t

type, public :: statement_list_t
    type(statement_t), allocatable :: s(:)
    integer :: n = 0
contains
    procedure :: push
end type statement_list_t

contains

subroutine split_statements(src, tokens, stmts)
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_list_t), intent(out) :: stmts
    integer :: i, lend, start, semi, kind

    allocate (stmts%s(64))
    i = 1
    do while (i <= tokens%n)
        lend = line_end_token(tokens, i)
        kind = line_kind(tokens, i, lend)
        if (kind /= SK_CODE) then
            call stmts%push(statement_t(kind, i, lend))
            i = lend + 1
            cycle
        end if

        start = i
        do
            ! a `;` with more code after it ends the statement within the line
            semi = find_semicolon(src, tokens, i, lend)
            if (semi > 0) then
                call stmts%push(statement_t(SK_CODE, start, semi))
                i = semi + 1
                start = i
                cycle
            end if
            if (.not. ends_with_continuation(tokens, i, lend)) exit
            ! continued: take in the next code line and any comment, blank or preprocessor lines before it
            do
                i = lend + 1
                if (i > tokens%n) exit
                lend = line_end_token(tokens, i)
                if (line_kind(tokens, i, lend) == SK_CODE) exit
            end do
            if (i > tokens%n) then
                lend = tokens%n
                exit
            end if
        end do
        call stmts%push(statement_t(SK_CODE, start, lend))
        i = lend + 1
    end do
end subroutine split_statements

function code_tokens(src, tokens, stmt) result(idx)
    !! Indices of the code tokens of a statement: no blanks, line breaks, comments, continuation
    !! markers, preprocessor lines or `;` separators.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_t), intent(in) :: stmt
    integer, allocatable :: idx(:)
    integer :: k, n

    allocate (idx(stmt%last - stmt%first + 1))
    n = 0
    do k = stmt%first, stmt%last
        if (.not. is_code_kind(tokens%tok(k)%kind)) cycle
        if (tokens%tok(k)%kind == TK_OP) then
            if (src(tokens%tok(k)%first:tokens%tok(k)%last) == ';') cycle
        end if
        n = n + 1
        idx(n) = k
    end do
    idx = idx(1:n)
end function code_tokens

pure integer function line_end_token(tokens, i) result(j)
    !! Index of the line break that ends the line containing token `i`, or the last token.
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: i
    do j = i, tokens%n
        if (tokens%tok(j)%kind == TK_NEWLINE) return
    end do
    j = tokens%n
end function line_end_token

pure integer function line_kind(tokens, i, lend) result(kind)
    !! Kind of the line `i:lend` from its first non-blank token.
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: i, lend
    integer :: k
    kind = SK_BLANK
    do k = i, lend
        select case (tokens%tok(k)%kind)
        case (TK_SPACE, TK_NEWLINE, TK_BOM)
            cycle
        case (TK_COMMENT)
            kind = SK_COMMENT
        case (TK_PREPROC)
            kind = SK_PREPROC
        case default
            kind = SK_CODE
        end select
        return
    end do
end function line_kind

pure integer function find_semicolon(src, tokens, i, lend) result(semi)
    !! A `;` in `i:lend` that is followed by more code on the line, or 0.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: i, lend
    integer :: k, m
    do k = i, lend
        if (tokens%tok(k)%kind /= TK_OP) cycle
        if (src(tokens%tok(k)%first:tokens%tok(k)%last) /= ';') cycle
        do m = k + 1, lend
            select case (tokens%tok(m)%kind)
            case (TK_SPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                semi = k
                return
            end select
        end do
    end do
    semi = 0
end function find_semicolon

pure logical function ends_with_continuation(tokens, i, lend)
    !! True if the last token of the line before any comment is a continuation `&`.
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: i, lend
    integer :: k
    ends_with_continuation = .false.
    do k = lend, i, -1
        select case (tokens%tok(k)%kind)
        case (TK_SPACE, TK_NEWLINE, TK_COMMENT)
            cycle
        case (TK_CONT)
            ends_with_continuation = tokens%tok(k)%sub == CT_TRAILING
            return
        case default
            return
        end select
    end do
end function ends_with_continuation

subroutine push(self, s)
    class(statement_list_t), intent(inout) :: self
    type(statement_t), intent(in) :: s
    type(statement_t), allocatable :: tmp(:)
    if (.not. allocated(self%s)) allocate (self%s(64))
    if (self%n == size(self%s)) then
        allocate (tmp(2*self%n))
        tmp(1:self%n) = self%s(1:self%n)
        call move_alloc(tmp, self%s)
    end if
    self%n = self%n + 1
    self%s(self%n) = s
end subroutine push

end module ffmt_statements
