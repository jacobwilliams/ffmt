module ffmt_statements
!! Groups tokens into statements and standalone lines.
!!
!! A code statement runs from its first token to the line break that ends it, including any
!! continuation lines and the comment, blank and preprocessor lines between them. Statements
!! separated by `;` on one line are split after the `;`. Lines outside statements that hold only
!! blanks, a comment or a preprocessor directive become items of their own. Together the items
!! cover every token exactly once, in order.
use ffmt_tokens
use ffmt_strings, only: make_lowercase
implicit none
private

public :: split_statements, code_tokens, statement_view, set_view

! item kinds
integer, parameter, public :: SK_CODE = 1      !! a code statement
integer, parameter, public :: SK_BLANK = 2     !! a line with only blanks
integer, parameter, public :: SK_COMMENT = 3   !! a line with only a comment
integer, parameter, public :: SK_PREPROC = 4   !! a preprocessor directive line

type, public :: statement_t
    !! An item: a code statement or a standalone line, as a range of the token list.
    integer :: kind = SK_CODE   !! one of the SK_* constants
    integer :: first = 1    !! first token
    integer :: last = 0     !! last token
end type statement_t

type, public :: statement_list_t
    !! The items of a source text, in order.
    type(statement_t), allocatable :: s(:)   !! storage; only `s(1:n)` is in use
    integer :: n = 0                         !! number of items
contains
    procedure :: push                        !! append an item
end type statement_list_t

type, public :: statement_view_t
    !! The code tokens of one statement, with their lowercase text and bracket depth: what the rules
    !! that read a statement word by word need. The procedures take an index `j` into `code(:)`;
    !! outside `1:n` a word is `''` and its kind 0, so a rule can look before and after a token
    !! without checking the bounds.
    integer, allocatable :: code(:)         !! indices into the token list of the code tokens
                                            !! (see [[code_tokens]])
    integer :: n = 0                        !! number of code tokens
    integer :: first = 1                    !! first code token after a statement label (`10 continue`)
    integer, allocatable :: kinds(:)        !! kind of each code token (`TK_*`)
    integer, allocatable :: depth(:)        !! open `(` and `[` around each code token; a bracket is
                                            !! outside its own pair
    character(len=:), allocatable :: low    !! lowercase text of the code tokens, one after another
    integer, allocatable :: lo(:)           !! where the text of each code token starts in `low`
    integer, allocatable :: hi(:)           !! where it ends
contains
    procedure :: word => view_word          !! lowercase text of a code token
    procedure :: is => view_is              !! whether a code token's lowercase text is a given one
    procedure :: kind_of => view_kind_of    !! kind of a code token
    procedure :: closing => view_closing    !! the `)` that closes a `(`
end type statement_view_t

contains

subroutine split_statements(src, tokens, stmts)
    !! Group `tokens` into code statements and standalone blank, comment and preprocessor lines.
    character(len=*), intent(in) :: src              !! source text
    type(token_list_t), intent(in) :: tokens         !! tokens of `src`
    type(statement_list_t), intent(out) :: stmts     !! the items, covering every token once
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
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    type(statement_t), intent(in) :: stmt        !! the statement
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

function statement_view(src, tokens, stmt) result(v)
    !! The view of code statement `stmt` (see [[set_view]], which fills one in place, without a copy).
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    type(statement_t), intent(in) :: stmt        !! the statement
    type(statement_view_t) :: v
    call set_view(src, tokens, stmt, v)
end function statement_view

subroutine set_view(src, tokens, stmt, v)
    !! Make `v` the view of code statement `stmt`.
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    type(statement_t), intent(in) :: stmt        !! the statement
    type(statement_view_t), intent(out) :: v     !! the view
    integer :: j, p, d, total

    ! the code tokens, as code_tokens finds them, counted first to allocate once
    v%n = 0
    do j = stmt%first, stmt%last
        if (is_statement_code(j)) v%n = v%n + 1
    end do
    allocate (v%code(v%n), v%kinds(v%n), v%depth(v%n), v%lo(v%n), v%hi(v%n))
    p = 0
    do j = stmt%first, stmt%last
        if (.not. is_statement_code(j)) cycle
        p = p + 1
        v%code(p) = j
    end do
    total = 0
    do j = 1, v%n
        total = total + tokens%tok(v%code(j))%last - tokens%tok(v%code(j))%first + 1
    end do
    allocate (character(len=total) :: v%low)
    p = 0
    d = 0
    do j = 1, v%n
        associate (t => tokens%tok(v%code(j)))
            v%kinds(j) = t%kind
            v%lo(j) = p + 1
            v%low(p + 1:p + t%last - t%first + 1) = src(t%first:t%last)
            call make_lowercase(v%low(p + 1:p + t%last - t%first + 1))
            p = p + t%last - t%first + 1
            v%hi(j) = p
        end associate
        v%depth(j) = d
        if (v%kinds(j) /= TK_OP) cycle
        select case (v%low(v%lo(j):v%hi(j)))
        case ('(', '[')
            d = d + 1
        case (')', ']')
            d = max(0, d - 1)
            v%depth(j) = d
        end select
    end do
    if (v%n > 1) then
        if (v%kinds(1) == TK_INT) v%first = 2
    end if

contains

    logical function is_statement_code(k)
        !! True if token `k` is one of the code tokens of the statement (not a `;` separator).
        integer, intent(in) :: k   !! index into the token list
        is_statement_code = is_code_kind(tokens%tok(k)%kind)
        if (is_statement_code .and. tokens%tok(k)%kind == TK_OP) &
            is_statement_code = src(tokens%tok(k)%first:tokens%tok(k)%last) /= ';'
    end function is_statement_code

end subroutine set_view

pure function view_word(v, j) result(w)
    !! Lowercase text of code token `j`, or `''` outside the statement.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    character(len=:), allocatable :: w
    if (j < 1 .or. j > v%n) then
        w = ''
    else
        w = v%low(v%lo(j):v%hi(j))
    end if
end function view_word

pure logical function view_is(v, j, text)
    !! True if the lowercase text of code token `j` is `text`, without making a copy of it. Outside
    !! the statement only `''` matches.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    character(len=*), intent(in) :: text       !! the text, in lowercase
    if (j < 1 .or. j > v%n) then
        view_is = len(text) == 0
    else
        view_is = v%hi(j) - v%lo(j) + 1 == len(text)
        if (view_is) view_is = v%low(v%lo(j):v%hi(j)) == text
    end if
end function view_is

pure integer function view_kind_of(v, j) result(kind)
    !! Kind (`TK_*`) of code token `j`, or 0 outside the statement.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    kind = 0
    if (j >= 1 .and. j <= v%n) kind = v%kinds(j)
end function view_kind_of

pure integer function view_closing(v, j) result(c)
    !! Index of the `)` that closes the `(` at code token `j` (counting only parentheses), or 0 if
    !! token `j` is not `(` or it is not closed.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    integer :: d
    if (.not. v%is(j, '(')) then
        c = 0
        return
    end if
    d = 0
    do c = j, v%n
        if (v%is(c, '(')) then
            d = d + 1
        else if (v%is(c, ')')) then
            d = d - 1
            if (d == 0) return
        end if
    end do
    c = 0
end function view_closing

pure integer function line_end_token(tokens, i) result(j)
    !! Index of the line break that ends the line containing token `i`, or the last token.
    type(token_list_t), intent(in) :: tokens   !! the token list
    integer, intent(in) :: i                   !! a token at or after the start of the line
    do j = i, tokens%n
        if (tokens%tok(j)%kind == TK_NEWLINE) return
    end do
    j = tokens%n
end function line_end_token

pure integer function line_kind(tokens, i, lend) result(kind)
    !! Kind of the line `i:lend` from its first non-blank token.
    type(token_list_t), intent(in) :: tokens   !! the token list
    integer, intent(in) :: i, lend             !! first and last token of the line
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
    character(len=*), intent(in) :: src        !! source text
    type(token_list_t), intent(in) :: tokens   !! tokens of `src`
    integer, intent(in) :: i, lend             !! tokens to search, up to the end of the line
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
    type(token_list_t), intent(in) :: tokens   !! the token list
    integer, intent(in) :: i, lend             !! tokens to search, up to the end of the line
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
    !! Append item `s`, growing the storage as needed.
    class(statement_list_t), intent(inout) :: self   !! the item list
    type(statement_t), intent(in) :: s               !! item to append
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
