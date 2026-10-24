module ffmt_use_sort
!! Sorting `use` statements, and the names in their `only:` lists, alphabetically ignoring case.
!!
!! A group of `use` statements is a run of them with nothing between them: a blank line, comment,
!! preprocessor line or any other statement ends the group, so statements in different `#if`
!! branches never mix. A statement moves with its continuation lines and trailing comment.
!! Statements that share a line with another (`;`) or that are protected by formatter directives
!! stay where they are and end the group. Statements of one group that use the same module keep
!! their order.
!!
!! The names of an `only:` list are sorted by their local name (`c => x` sorts as `c`). A list that
!! is not sorted yet is written on one line, for line splitting to break again. A statement with a
!! comment or preprocessor line before its last line is left alone.
use ffmt_tokens
use ffmt_lexer, only: tokenize
use ffmt_statements
use ffmt_directives, only: protected_items
use ffmt_strings, only: string_t, string_builder_t
implicit none
private

public :: sort_use, is_use_statement, only_items, sorted_order

contains

function sort_use(src, statements, only_lists) result(out)
    !! `src` with the `use` statements of each group sorted by module name (if `statements`) and
    !! the names of `only:` lists sorted (if `only_lists`).
    character(len=*), intent(in) :: src
    logical, intent(in) :: statements
    logical, intent(in) :: only_lists
    character(len=:), allocatable :: out

    type(token_list_t) :: tokens
    type(statement_list_t) :: stmts
    logical, allocatable :: keep(:), movable(:)
    type(string_t), allocatable :: text(:), keys(:)
    integer, allocatable :: code(:), order(:)
    type(string_builder_t) :: buf
    integer :: k, m, j

    if (.not. (statements .or. only_lists)) then
        out = src
        return
    end if
    call tokenize(src, tokens)
    call split_statements(src, tokens, stmts)
    keep = protected_items(src, tokens, stmts)
    allocate (text(stmts%n), keys(stmts%n), movable(stmts%n))
    do k = 1, stmts%n
        text(k)%s = src(tokens%tok(stmts%s(k)%first)%first:tokens%tok(stmts%s(k)%last)%last)
        keys(k)%s = ''
        movable(k) = .false.
        if (stmts%s(k)%kind /= SK_CODE .or. keep(k)) cycle
        code = code_tokens(src, tokens, stmts%s(k))
        if (.not. is_use_statement(src, tokens, code)) cycle
        if (only_lists) call sort_only_list(src, tokens, stmts%s(k), code, text(k)%s)
        keys(k)%s = module_key(src, tokens, code)
        movable(k) = statements .and. whole_lines(tokens, stmts%s(k))
    end do

    k = 1
    do while (k <= stmts%n)
        if (.not. movable(k)) then
            call buf%append(text(k)%s)
            k = k + 1
            cycle
        end if
        m = k
        do while (m < stmts%n)
            if (.not. movable(m + 1)) exit
            m = m + 1
        end do
        order = sorted_order(keys(k:m))
        do j = 1, size(order)
            call buf%append(text(k + order(j) - 1)%s)
        end do
        k = m + 1
    end do
    out = buf%get()
end function sort_use

logical function is_use_statement(src, tokens, code)
    !! True if the code tokens `code` of a statement form a `use` statement.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: code(:)
    character(len=:), allocatable :: second

    is_use_statement = .false.
    if (size(code) < 2) return
    if (tokens%tok(code(1))%kind /= TK_NAME) return
    if (token_lower(src, tokens%tok(code(1))) /= 'use') return
    if (tokens%tok(code(2))%kind == TK_NAME) then
        is_use_statement = .true.
    else if (tokens%tok(code(2))%kind == TK_OP) then
        second = token_text(src, tokens%tok(code(2)))
        is_use_statement = second == ',' .or. second == '::'
    end if
end function is_use_statement

function only_items(src, tokens, code) result(starts)
    !! Positions in `code` where the items of a `use` statement's `only:` list start. Each item
    !! ends before the comma in front of the next one, the last at the end of `code`. Empty if there
    !! is no `only:` list, the list is empty, or it has an empty item.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: code(:)
    integer, allocatable :: starts(:)
    integer :: p, q, n, depth
    character(len=:), allocatable :: t

    allocate (starts(0))
    n = size(code)
    do p = 3, n - 1
        if (is_text(code(p - 1), ',') .and. is_text(code(p + 1), ':') .and. &
            token_lower(src, tokens%tok(code(p))) == 'only') exit
    end do
    if (p >= n .or. p + 2 > n) return
    starts = [p + 2]
    depth = 0
    do q = p + 2, n
        if (tokens%tok(code(q))%kind /= TK_OP) cycle
        t = token_text(src, tokens%tok(code(q)))
        if (t == '(' .or. t == '[') then
            depth = depth + 1
        else if (t == ')' .or. t == ']') then
            depth = depth - 1
        else if (t == ',' .and. depth == 0) then
            if (q == n .or. q == starts(size(starts))) then   ! an empty item
                deallocate (starts)
                allocate (starts(0))
                return
            end if
            starts = [starts, q + 1]
        end if
    end do

contains

    logical function is_text(j, text)
        integer, intent(in) :: j
        character(len=*), intent(in) :: text
        is_text = tokens%tok(j)%kind == TK_OP
        if (is_text) is_text = token_text(src, tokens%tok(j)) == text
    end function is_text

end function only_items

function sorted_order(keys) result(order)
    !! The order in which `keys` are sorted (a stable sort, comparing in ASCII order).
    type(string_t), intent(in) :: keys(:)
    integer, allocatable :: order(:)
    integer :: i, j, t

    order = [(i, i=1, size(keys))]
    do i = 2, size(keys)
        t = order(i)
        j = i - 1
        do while (j >= 1)
            if (.not. llt(keys(t)%s, keys(order(j))%s)) exit
            order(j + 1) = order(j)
            j = j - 1
        end do
        order(j + 1) = t
    end do
end function sorted_order

subroutine sort_only_list(src, tokens, stmt, code, text)
    !! Replace `text`, the text of `stmt`, by the statement with its `only:` list sorted, unless the
    !! list is sorted already or the statement cannot be rewritten safely.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_t), intent(in) :: stmt
    integer, intent(in) :: code(:)
    character(len=:), allocatable, intent(inout) :: text

    integer, allocatable :: starts(:), ends(:), order(:)
    type(string_t), allocatable :: keys(:)
    type(string_builder_t) :: buf
    character(len=:), allocatable :: sep
    logical :: commented
    integer :: j, n, ns

    starts = only_items(src, tokens, code)
    ns = size(starts)
    if (ns < 2) return
    n = size(code)

    ! comments are only allowed on the last line; no preprocessor lines or tokens split across lines
    commented = .false.
    do j = stmt%first, stmt%last
        select case (tokens%tok(j)%kind)
        case (TK_COMMENT)
            commented = .true.
        case (TK_NEWLINE)
            if (commented .and. j /= stmt%last) return
        case (TK_PREPROC)
            return
        end select
    end do
    if (any(tokens%tok(code(starts(1):n))%glued)) return

    ends = [starts(2:) - 2, n]
    allocate (keys(ns))
    do j = 1, ns
        keys(j)%s = local_name(starts(j), ends(j))
    end do
    order = sorted_order(keys)
    if (all(order == [(j, j=1, ns)])) return

    call buf%append(src(tokens%tok(stmt%first)%first:tokens%tok(code(starts(1) - 1))%last))
    if (separated(starts(1))) call buf%append(' ')
    sep = ','
    if (separated(starts(2))) sep = ', '
    do j = 1, ns
        if (j > 1) call buf%append(sep)
        call buf%append(joined(starts(order(j)), ends(order(j))))
    end do
    call buf%append(src(tokens%tok(code(n))%last + 1:tokens%tok(stmt%last)%last))
    text = buf%get()

contains

    logical function separated(p)
        !! True if anything (blanks, a line break) comes between code tokens `p - 1` and `p`.
        integer, intent(in) :: p
        separated = code(p) /= code(p - 1) + 1
    end function separated

    function joined(first, last) result(s)
        !! The text of code tokens `first:last`, with a blank wherever they were separated.
        integer, intent(in) :: first, last
        character(len=:), allocatable :: s
        integer :: p
        s = token_text(src, tokens%tok(code(first)))
        do p = first + 1, last
            if (separated(p)) s = s//' '
            s = s//token_text(src, tokens%tok(code(p)))
        end do
    end function joined

    function local_name(first, last) result(s)
        !! Sort key of an item: its tokens before `=>`, in lowercase.
        integer, intent(in) :: first, last
        character(len=:), allocatable :: s
        integer :: p
        s = ''
        do p = first, last
            if (tokens%tok(code(p))%kind == TK_OP) then
                if (token_text(src, tokens%tok(code(p))) == '=>') exit
            end if
            s = s//token_lower(src, tokens%tok(code(p)))
        end do
    end function local_name

end subroutine sort_only_list

function module_key(src, tokens, code) result(key)
    !! Sort key of a `use` statement: the module name in lowercase.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    integer, intent(in) :: code(:)
    character(len=:), allocatable :: key
    character(len=:), allocatable :: t
    integer :: p

    p = 2
    t = token_text(src, tokens%tok(code(2)))
    if (t == '::') then
        p = 3
    else if (t == ',') then
        ! `use, intrinsic :: name`
        do p = 3, size(code)
            if (token_text(src, tokens%tok(code(p - 1))) == '::') exit
        end do
    end if
    key = ''
    do while (p <= size(code))
        if (token_text(src, tokens%tok(code(p))) == ',') exit
        key = key//token_lower(src, tokens%tok(code(p)))
        p = p + 1
    end do
end function module_key

pure logical function whole_lines(tokens, stmt)
    !! True if `stmt` takes up whole lines on its own, ending with a line break, so that it can
    !! be moved.
    type(token_list_t), intent(in) :: tokens
    type(statement_t), intent(in) :: stmt
    whole_lines = .false.
    if (tokens%tok(stmt%last)%kind /= TK_NEWLINE) return
    if (stmt%first > 1) then
        if (tokens%tok(stmt%first - 1)%kind /= TK_NEWLINE) return
    end if
    whole_lines = .not. any(tokens%tok(stmt%first:stmt%last)%kind == TK_PREPROC .or. &
                            tokens%tok(stmt%first:stmt%last)%kind == TK_BOM)
end function whole_lines

end module ffmt_use_sort
