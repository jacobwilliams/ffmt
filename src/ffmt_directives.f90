module ffmt_directives
!! Formatter directives in comments, which protect code from formatting:
!!
!! * `!&` at the end of a statement keeps the statement exactly as it is.
!! * `!&<` on a line of its own starts a region that is kept exactly as it is, and `!&>` ends it.
use ffmt_tokens
use ffmt_statements
implicit none
private

public :: protected_items

integer, parameter :: DIR_NONE = 0, DIR_KEEP = 1, DIR_OFF = 2, DIR_ON = 3

contains

function protected_items(src, tokens, stmts) result(keep)
    !! True for each item that must be written exactly as it is: statements with a `!&` comment, and
    !! everything from a `!&<` line to the next `!&>` line, both included.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_list_t), intent(in) :: stmts
    logical, allocatable :: keep(:)
    logical :: off
    integer :: k, j, d

    allocate (keep(stmts%n))
    off = .false.
    do k = 1, stmts%n
        keep(k) = off
        do j = stmts%s(k)%first, stmts%s(k)%last
            if (tokens%tok(j)%kind /= TK_COMMENT) cycle
            d = directive(src, tokens%tok(j))
            if (stmts%s(k)%kind == SK_COMMENT .and. d == DIR_OFF) then
                off = .true.
                keep(k) = .true.
            else if (stmts%s(k)%kind == SK_COMMENT .and. d == DIR_ON) then
                off = .false.
                keep(k) = .true.
            else if (stmts%s(k)%kind == SK_CODE .and. d == DIR_KEEP) then
                keep(k) = .true.
            end if
        end do
    end do
end function protected_items

integer function directive(src, t)
    !! Formatter directive of a comment token.
    character(len=*), intent(in) :: src
    type(token_t), intent(in) :: t
    character(len=:), allocatable :: text
    integer :: n
    directive = DIR_NONE
    if (t%sub /= CM_FORMATTER) return
    text = token_text(src, t)
    n = len(text)
    do while (n > 0)                        ! without trailing blanks and tabs
        if (text(n:n) /= ' ' .and. text(n:n) /= achar(9)) exit
        n = n - 1
    end do
    text = text(1:n)
    if (text == '!&<') then
        directive = DIR_OFF
    else if (text == '!&>') then
        directive = DIR_ON
    else if (len(text) == 2) then
        directive = DIR_KEEP
    else if (text(3:3) == ' ') then
        directive = DIR_KEEP
    end if
end function directive

end module ffmt_directives
