module fregex_compiler
!! Compiles a parsed regular expression (AST) into a VM program.
use fregex_parser
use fregex_program
use fregex_charset, only: charset_t, full_set
use fregex_string, only: to_lower_code, is_alpha_code
implicit none
private

public :: compile_ast

integer, parameter :: MAX_PROGRAM_SIZE = 1000000
integer, parameter :: BIG_WIDTH = huge(1)/4   !! widths above this are treated as unbounded

contains

subroutine compile_ast(ast, prog, errmsg)
    type(ast_t), intent(in) :: ast
    type(program_t), intent(out) :: prog
    character(len=:), allocatable, intent(out) :: errmsg
    integer :: pc
    logical :: nullable

    prog%sets = ast%sets(1:ast%nsets)
    prog%ngroups = ast%ngroups
    prog%names = ast%names

    pc = prog%emit(OP_SAVE, 0)
    call emit_node(ast, prog, ast%root, errmsg)
    if (allocated(errmsg)) return
    pc = prog%emit(OP_SAVE, 1)
    pc = prog%emit(OP_MATCH)

    prog%anchored = is_anchored(ast, ast%root)
    call first_set(ast, ast%root, prog%first, nullable)
    prog%use_first = .not. nullable
end subroutine compile_ast

recursive subroutine emit_node(ast, prog, id, errmsg)
    type(ast_t), intent(in) :: ast
    type(program_t), intent(inout) :: prog
    integer, intent(in) :: id
    character(len=:), allocatable, intent(inout) :: errmsg
    integer :: k, pc, wmin, wmax, reg
    integer, allocatable :: jumps(:)

    if (allocated(errmsg)) return
    if (prog%n > MAX_PROGRAM_SIZE) then
        errmsg = 'pattern too large after expanding repeat counts'
        return
    end if

    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_EMPTY)
            continue
        case (N_CHAR)
            if (node%b == 1) then
                pc = prog%emit(OP_CHARI, to_lower_code(node%a))
            else
                pc = prog%emit(OP_CHAR, node%a)
            end if
        case (N_ANY)
            pc = prog%emit(merge(OP_ANYNL, OP_ANY, node%a == 1))
        case (N_SET)
            pc = prog%emit(OP_SET, node%a)
        case (N_ASSERT)
            pc = prog%emit(assert_op(node%a))
        case (N_CONCAT)
            do k = 1, size(node%kids)
                call emit_node(ast, prog, node%kids(k), errmsg)
            end do
        case (N_ALT)
            ! SPLIT next, alt2; body1; JMP end; SPLIT next, alt3; body2; JMP end; ... bodyN
            allocate (jumps(size(node%kids) - 1))
            do k = 1, size(node%kids) - 1
                pc = prog%emit(OP_SPLIT, prog%n + 2)
                call emit_node(ast, prog, node%kids(k), errmsg)
                jumps(k) = prog%emit(OP_JMP)
                prog%code(2, pc) = prog%n + 1
            end do
            call emit_node(ast, prog, node%kids(size(node%kids)), errmsg)
            do k = 1, size(jumps)
                prog%code(1, jumps(k)) = prog%n + 1
            end do
        case (N_GROUP)
            pc = prog%emit(OP_SAVE, 2*node%a)
            call emit_node(ast, prog, node%kids(1), errmsg)
            pc = prog%emit(OP_SAVE, 2*node%a + 1)
        case (N_REPEAT)
            if (node%c == R_POSSESSIVE) then
                pc = prog%emit(OP_ATOMIC)
                call emit_repeat(ast, prog, node%kids(1), node%a, node%b, .true., errmsg)
                k = prog%emit(OP_SUCCEED)
                prog%code(1, pc) = prog%n + 1
            else
                call emit_repeat(ast, prog, node%kids(1), node%a, node%b, node%c == R_GREEDY, errmsg)
            end if
        case (N_LOOK)
            if (node%a == 1) then
                pc = prog%emit(OP_LOOKAHEAD, 0, node%b)
                call emit_node(ast, prog, node%kids(1), errmsg)
            else
                call node_width(ast, node%kids(1), wmin, wmax)
                reg = prog%new_reg()
                pc = prog%emit(OP_LOOKBEHIND, 0, node%b, wmin, merge(UNBOUNDED, wmax, wmax == INF), reg)
                call emit_node(ast, prog, node%kids(1), errmsg)
                k = prog%emit(OP_ENDAT, reg)
            end if
            k = prog%emit(OP_SUCCEED)
            prog%code(1, pc) = prog%n + 1
        case (N_ATOMIC)
            pc = prog%emit(OP_ATOMIC)
            call emit_node(ast, prog, node%kids(1), errmsg)
            k = prog%emit(OP_SUCCEED)
            prog%code(1, pc) = prog%n + 1
        case (N_BACKREF)
            pc = prog%emit(OP_BACKREF, node%a, node%b)
        end select
    end associate
end subroutine emit_node

recursive subroutine emit_repeat(ast, prog, body, qmin, qmax, greedy, errmsg)
    !! Emit `body{qmin,qmax}`.
    !!
    !! The first `qmin` copies are emitted unconditionally. The rest are either a loop (unbounded) or
    !! `qmax-qmin` optional copies. If the body can match the empty string, an iteration that consumes
    !! nothing ends the repetition (as in Python's `re`), which also prevents infinite loops.
    type(ast_t), intent(in) :: ast
    type(program_t), intent(inout) :: prog
    integer, intent(in) :: body, qmin, qmax
    logical, intent(in) :: greedy
    character(len=:), allocatable, intent(inout) :: errmsg
    integer :: k, pc, loop, reg, exit_pc
    integer, allocatable :: splits(:), checks(:)
    logical :: nullable

    nullable = can_be_empty(ast, body)
    reg = 0
    if (nullable) reg = prog%new_reg()

    do k = 1, qmin
        call emit_node(ast, prog, body, errmsg)
        if (allocated(errmsg)) return
    end do

    if (qmax == INF) then
        allocate (splits(1), checks(0))
        loop = prog%emit(OP_SPLIT)
        splits(1) = loop
        call emit_iteration()
        pc = prog%emit(OP_JMP, loop)
    else
        allocate (splits(qmax - qmin), checks(0))
        do k = 1, qmax - qmin
            splits(k) = prog%emit(OP_SPLIT)
            call emit_iteration()
            if (allocated(errmsg)) return
        end do
    end if

    exit_pc = prog%n + 1
    do k = 1, size(splits)
        if (greedy) then
            prog%code(1:2, splits(k)) = [splits(k) + 1, exit_pc]
        else
            prog%code(1:2, splits(k)) = [exit_pc, splits(k) + 1]
        end if
    end do
    do k = 1, size(checks)
        prog%code(2, checks(k)) = exit_pc
    end do

contains

    recursive subroutine emit_iteration()
        if (nullable) pc = prog%emit(OP_MARK, reg)
        call emit_node(ast, prog, body, errmsg)
        if (nullable) then
            pc = prog%emit(OP_CHECK_EXIT, reg)
            checks = [checks, pc]
        end if
    end subroutine emit_iteration

end subroutine emit_repeat

pure integer function assert_op(kind) result(op)
    integer, intent(in) :: kind
    select case (kind)
    case (A_BOL)
        op = OP_BOL
    case (A_BOL_M)
        op = OP_BOL_M
    case (A_EOL)
        op = OP_EOL
    case (A_EOL_M)
        op = OP_EOL_M
    case (A_BOS)
        op = OP_BOS
    case (A_EOS)
        op = OP_EOS
    case (A_WORDB)
        op = OP_WORDB
    case default
        op = OP_NWORDB
    end select
end function assert_op

logical function can_be_empty(ast, id)
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    integer :: wmin, wmax
    call node_width(ast, id, wmin, wmax)
    can_be_empty = wmin == 0
end function can_be_empty

recursive subroutine node_width(ast, id, wmin, wmax)
    !! Minimum and maximum number of bytes a node can match (`wmax` = INF if unbounded).
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    integer, intent(out) :: wmin, wmax
    integer :: k, kmin, kmax

    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_CHAR, N_ANY, N_SET)
            wmin = 1
            wmax = 1
        case (N_CONCAT)
            wmin = 0
            wmax = 0
            do k = 1, size(node%kids)
                call node_width(ast, node%kids(k), kmin, kmax)
                wmin = add_width(wmin, kmin)
                wmax = add_width(wmax, kmax)
            end do
        case (N_ALT)
            call node_width(ast, node%kids(1), wmin, wmax)
            do k = 2, size(node%kids)
                call node_width(ast, node%kids(k), kmin, kmax)
                wmin = min(wmin, kmin)
                if (wmax /= INF) wmax = merge(INF, max(wmax, kmax), kmax == INF)
            end do
        case (N_GROUP, N_ATOMIC)
            call node_width(ast, node%kids(1), wmin, wmax)
        case (N_REPEAT)
            call node_width(ast, node%kids(1), kmin, kmax)
            wmin = mul_width(kmin, node%a)
            if (kmax == 0 .or. node%b == 0) then
                wmax = 0
            else if (kmax == INF .or. node%b == INF) then
                wmax = INF
            else
                wmax = mul_width(kmax, node%b)
            end if
        case (N_BACKREF)
            wmin = 0
            wmax = INF
        case default   ! N_EMPTY, N_ASSERT, N_LOOK
            wmin = 0
            wmax = 0
        end select
    end associate
end subroutine node_width

pure integer function add_width(a, b) result(w)
    integer, intent(in) :: a, b
    if (a == INF .or. b == INF) then
        w = INF
    else
        w = min(a + b, BIG_WIDTH)
        if (w == BIG_WIDTH) w = INF
    end if
end function add_width

pure integer function mul_width(a, b) result(w)
    integer, intent(in) :: a, b
    if (a == 0 .or. b == 0) then
        w = 0
    else if (a > BIG_WIDTH/b) then
        w = INF
    else
        w = a*b
    end if
end function mul_width

recursive logical function is_anchored(ast, id) result(anchored)
    !! True if every match must start at the beginning of the string (`\A...` or `^...` without MULTILINE).
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    integer :: k

    anchored = .false.
    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_ASSERT)
            anchored = node%a == A_BOS .or. node%a == A_BOL
        case (N_CONCAT)
            anchored = is_anchored(ast, node%kids(1))
        case (N_GROUP, N_ATOMIC)
            anchored = is_anchored(ast, node%kids(1))
        case (N_REPEAT)
            anchored = node%a >= 1 .and. is_anchored(ast, node%kids(1))
        case (N_ALT)
            anchored = .true.
            do k = 1, size(node%kids)
                if (.not. is_anchored(ast, node%kids(k))) then
                    anchored = .false.
                    exit
                end if
            end do
        end select
    end associate
end function is_anchored

recursive subroutine first_set(ast, id, set, nullable)
    !! Add to `set` every byte a match of node `id` can start with. `nullable` is true if the
    !! node can match without consuming anything, in which case the following node's first
    !! bytes matter too. Zero-width assertions are treated as transparent, so the result is a
    !! superset, which is all the search loop needs.
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    type(charset_t), intent(inout) :: set
    logical, intent(out) :: nullable
    integer :: k, c
    logical :: kid_nullable
    type(charset_t) :: any_set

    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_CHAR)
            call set%add(node%a)
            if (node%b == 1) then
                c = to_lower_code(node%a)
                call set%add(c)
                call set%add(c - 32)
            end if
            nullable = .false.
        case (N_ANY)
            any_set = full_set()
            call set%add_set(any_set)
            nullable = .false.
        case (N_SET)
            call set%add_set(ast%sets(node%a))
            nullable = .false.
        case (N_CONCAT)
            nullable = .true.
            do k = 1, size(node%kids)
                call first_set(ast, node%kids(k), set, kid_nullable)
                if (.not. kid_nullable) then
                    nullable = .false.
                    exit
                end if
            end do
        case (N_ALT)
            nullable = .false.
            do k = 1, size(node%kids)
                call first_set(ast, node%kids(k), set, kid_nullable)
                nullable = nullable .or. kid_nullable
            end do
        case (N_GROUP, N_ATOMIC)
            call first_set(ast, node%kids(1), set, nullable)
        case (N_REPEAT)
            if (node%b == 0) then
                nullable = .true.
            else
                call first_set(ast, node%kids(1), set, kid_nullable)
                nullable = kid_nullable .or. node%a == 0
            end if
        case (N_BACKREF)
            any_set = full_set()
            call set%add_set(any_set)
            nullable = .true.
        case default   ! N_EMPTY, N_ASSERT, N_LOOK
            nullable = .true.
        end select
    end associate
end subroutine first_set

end module fregex_compiler
