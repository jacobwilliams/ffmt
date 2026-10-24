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
!! is not sorted yet is rewritten: on one line, for line splitting to break again, or broken at
!! commas to fit a given width, with its continuation lines starting below the first name. A
!! statement with a comment or preprocessor line before its last line is left alone.
use ffmt_tokens
use ffmt_statements
use ffmt_source, only: source_t, parse_source
use ffmt_strings, only: string_t, string_builder_t
implicit none
private

public :: sort_use, is_use_statement, only_items, sorted_order

contains

function sort_use(src, statements, only_lists, width) result(out)
    !! `src` with the `use` statements of each group sorted by module name (if `statements`) and
    !! the names of `only:` lists sorted (if `only_lists`).
    character(len=*), intent(in) :: src          !! source text
    logical, intent(in) :: statements            !! sort the `use` statements of each group
    logical, intent(in) :: only_lists            !! sort the names of `only:` lists
    integer, intent(in) :: width   !! line length to fit a rewritten `only:` list into, or 0 to write it on one line
    character(len=:), allocatable :: out

    type(source_t) :: source
    type(statement_view_t) :: v
    logical, allocatable :: movable(:)
    type(string_t), allocatable :: text(:), keys(:)
    integer, allocatable :: order(:)
    type(string_builder_t) :: buf
    integer :: k, m, j

    if (.not. (statements .or. only_lists)) then
        out = src
        return
    end if
    call parse_source(src, source)
    associate (tokens => source%tokens, stmts => source%stmts)
        allocate (text(stmts%n), keys(stmts%n), movable(stmts%n))
        do k = 1, stmts%n
            text(k)%s = src(tokens%tok(stmts%s(k)%first)%first:tokens%tok(stmts%s(k)%last)%last)
            keys(k)%s = ''
            movable(k) = .false.
            if (stmts%s(k)%kind /= SK_CODE .or. source%keep(k)) cycle
            call set_view(src, tokens, stmts%s(k), v)
            if (.not. is_use_statement(v)) cycle
            if (only_lists) call sort_only_list(src, tokens, stmts%s(k), v, width, text(k)%s)
            keys(k)%s = module_key(v)
            movable(k) = statements .and. whole_lines(tokens, stmts%s(k))
        end do
    end associate

    k = 1
    do while (k <= size(movable))
        if (.not. movable(k)) then
            call buf%append(text(k)%s)
            k = k + 1
            cycle
        end if
        m = k
        do while (m < size(movable))
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

pure logical function is_use_statement(v)
    !! True if statement `v` is a `use` statement.
    type(statement_view_t), intent(in) :: v   !! the statement
    is_use_statement = .false.
    if (v%n < 2) return
    if (v%kinds(1) /= TK_NAME .or. .not. v%is(1, 'use')) return
    select case (v%kinds(2))
    case (TK_NAME)
        is_use_statement = .true.
    case (TK_OP)
        is_use_statement = v%is(2, ',') .or. v%is(2, '::')
    end select
end function is_use_statement

function only_items(v) result(starts)
    !! Positions in `v%code` where the items of a `use` statement's `only:` list start. Each item
    !! ends before the comma in front of the next one, the last at the end of the statement. Empty
    !! if there is no `only:` list, the list is empty, or it has an empty item.
    type(statement_view_t), intent(in) :: v   !! the `use` statement
    integer, allocatable :: starts(:)
    integer :: p, q, n, depth

    allocate (starts(0))
    n = v%n
    do p = 3, n - 1
        if (is_op(p - 1, ',') .and. is_op(p + 1, ':') .and. v%is(p, 'only')) exit
    end do
    if (p >= n .or. p + 2 > n) return
    starts = [p + 2]
    depth = 0
    do q = p + 2, n
        if (v%kinds(q) /= TK_OP) cycle
        if (v%is(q, '(') .or. v%is(q, '[')) then
            depth = depth + 1
        else if (v%is(q, ')') .or. v%is(q, ']')) then
            depth = depth - 1
        else if (v%is(q, ',') .and. depth == 0) then
            if (q == n .or. q == starts(size(starts))) then   ! an empty item
                deallocate (starts)
                allocate (starts(0))
                return
            end if
            starts = [starts, q + 1]
        end if
    end do

contains

    logical function is_op(j, text)
        !! True if code token `j` is the operator `text`.
        integer, intent(in) :: j                 !! index into `v%code(:)`
        character(len=*), intent(in) :: text     !! the operator, e.g. `,`
        is_op = v%kinds(j) == TK_OP .and. v%is(j, text)
    end function is_op

end function only_items

function sorted_order(keys) result(order)
    !! The order in which `keys` are sorted (a stable sort, comparing in ASCII order).
    type(string_t), intent(in) :: keys(:)   !! the sort keys
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

subroutine sort_only_list(src, tokens, stmt, v, width, text)
    !! Replace `text`, the text of `stmt`, by the statement with its `only:` list sorted, unless the
    !! list is sorted already or the statement cannot be rewritten safely. With `width > 0`, the
    !! list is broken after commas so that its lines fit in `width` columns where possible.
    character(len=*), intent(in) :: src                      !! source text
    type(token_list_t), intent(in) :: tokens                 !! tokens of `src`
    type(statement_t), intent(in) :: stmt                    !! the `use` statement
    type(statement_view_t), intent(in) :: v                  !! the view of `stmt`
    integer, intent(in) :: width                             !! line length to fit the list into, or 0
    character(len=:), allocatable, intent(inout) :: text     !! text of `stmt`, from its first to its last token

    integer, allocatable :: starts(:), ends(:), order(:)
    type(string_t), allocatable :: keys(:)
    type(string_builder_t) :: buf
    character(len=:), allocatable :: sep, prefix, item, nl
    logical :: commented
    integer :: j, n, ns, column, used

    starts = only_items(v)
    ns = size(starts)
    if (ns < 2) return
    n = v%n

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
    if (any(tokens%tok(v%code(starts(1):n))%glued)) return

    ends = [starts(2:) - 2, n]
    allocate (keys(ns))
    do j = 1, ns
        keys(j)%s = local_name(starts(j), ends(j))
    end do
    order = sorted_order(keys)
    if (all(order == [(j, j=1, ns)])) return

    prefix = src(tokens%tok(stmt%first)%first:tokens%tok(v%code(starts(1) - 1))%last)
    if (separated(starts(1))) prefix = prefix//' '
    call buf%append(prefix)
    sep = ','
    if (separated(starts(2))) sep = ', '
    nl = achar(10)
    if (tokens%tok(stmt%last)%kind == TK_NEWLINE) nl = token_text(src, tokens%tok(stmt%last))
    ! the column of the first name, where the continuation lines start too
    column = len(prefix) - max(index(prefix, achar(10), back=.true.), index(prefix, achar(13), back=.true.))
    used = column
    do j = 1, ns
        item = joined(starts(order(j)), ends(order(j)))
        if (j > 1) then
            ! room for the item and a `, &` after it
            if (width > 0 .and. used + len(sep) + len(item) + 3 > width) then
                call buf%append(', &'//nl//repeat(' ', column))
                used = column
            else
                call buf%append(sep)
                used = used + len(sep)
            end if
        end if
        call buf%append(item)
        used = used + len(item)
    end do
    call buf%append(src(tokens%tok(v%code(n))%last + 1:tokens%tok(stmt%last)%last))
    text = buf%get()

contains

    logical function separated(p)
        !! True if anything (blanks, a line break) comes between code tokens `p - 1` and `p`.
        integer, intent(in) :: p   !! index into `v%code(:)`
        separated = v%code(p) /= v%code(p - 1) + 1
    end function separated

    function joined(first, last) result(s)
        !! The text of code tokens `first:last`, with a blank wherever they were separated.
        integer, intent(in) :: first, last   !! range of indices into `v%code(:)`
        character(len=:), allocatable :: s
        integer :: p
        s = token_text(src, tokens%tok(v%code(first)))
        do p = first + 1, last
            if (separated(p)) s = s//' '
            s = s//token_text(src, tokens%tok(v%code(p)))
        end do
    end function joined

    function local_name(first, last) result(s)
        !! Sort key of an item: its tokens before `=>`, in lowercase.
        integer, intent(in) :: first, last   !! range of indices into `v%code(:)` of the item
        character(len=:), allocatable :: s
        integer :: p
        s = ''
        do p = first, last
            if (v%kinds(p) == TK_OP .and. v%is(p, '=>')) exit
            s = s//v%word(p)
        end do
    end function local_name

end subroutine sort_only_list

function module_key(v) result(key)
    !! Sort key of a `use` statement: the module name in lowercase.
    type(statement_view_t), intent(in) :: v   !! the `use` statement
    character(len=:), allocatable :: key
    integer :: p

    p = 2
    if (v%is(2, '::')) then
        p = 3
    else if (v%is(2, ',')) then
        ! `use, intrinsic :: name`
        do p = 3, v%n
            if (v%is(p - 1, '::')) exit
        end do
    end if
    key = ''
    do while (p <= v%n)
        if (v%is(p, ',')) exit
        key = key//v%word(p)
        p = p + 1
    end do
end function module_key

pure logical function whole_lines(tokens, stmt)
    !! True if `stmt` takes up whole lines on its own, ending with a line break, so that it can
    !! be moved.
    type(token_list_t), intent(in) :: tokens   !! tokens of the source
    type(statement_t), intent(in) :: stmt      !! the statement, a range of `tokens`
    whole_lines = .false.
    if (tokens%tok(stmt%last)%kind /= TK_NEWLINE) return
    if (stmt%first > 1) then
        if (tokens%tok(stmt%first - 1)%kind /= TK_NEWLINE) return
    end if
    whole_lines = .not. any(tokens%tok(stmt%first:stmt%last)%kind == TK_PREPROC .or. &
                            tokens%tok(stmt%first:stmt%last)%kind == TK_BOM)
end function whole_lines

end module ffmt_use_sort
