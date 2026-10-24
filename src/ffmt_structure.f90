module ffmt_structure
!! Block structure of a file: the nesting depth of every statement, from a stack of open
!! constructs, plus diagnostics for statements that do not fit (an `end` with nothing to close,
!! an `else` outside an `if`, a construct still open at the end of the file, ...).
use ffmt_tokens
use ffmt_statements
use ffmt_constructs
implicit none
private

public :: analyze_structure

type, public :: diagnostic_t
    !! A problem with the block structure, such as an `end` that matches no open construct.
    integer :: line = 0                       !! source line of the statement concerned
    character(len=:), allocatable :: message  !! what is wrong, e.g. `'if is not closed'`
end type diagnostic_t

type :: scope_t
    !! An open construct on the scope stack.
    integer :: construct = C_NONE   !! one of the `C_*` constants
    integer :: do_label = 0         !! for `do 10 ...`: the label of the statement that ends the loop, else 0
    integer :: line = 0             !! source line of the opening statement
end type scope_t

type :: saved_stack_t
    !! Scope stacks for one preprocessor conditional: as it was at the `#if` (restored at each
    !! `#elif` / `#else`) and as it was at the end of the first branch (used after `#endif`).
    type(scope_t), allocatable :: at_if(:)        !! the stack at the `#if`
    type(scope_t), allocatable :: after_first(:)  !! the stack at the end of the first branch; unallocated until then
end type saved_stack_t

! preprocessor conditionals
integer, parameter :: PP_OTHER = 0, PP_IF = 1, PP_ELSE = 2, PP_ENDIF = 3

contains

subroutine analyze_structure(src, tokens, stmts, info, depth, diags, units, procedures)
    !! Classify every statement and compute its depth: the number of constructs enclosing it.
    !! `units` is the number of those constructs that are program units (module, submodule or
    !! main program), and `procedures` the number that are procedure definitions (subroutine, function
    !! or separate module procedure, but not interface bodies), so callers can choose not to indent
    !! their bodies.
    !! An opening statement is at the depth outside its construct, the statements inside are one
    !! deeper, and middle (`else`, `contains`, ...) and closing statements are at the opening depth.
    !! Non-code items get the depth of the code around them.
    !!
    !! Branches of preprocessor conditionals (cpp `#if`/`#ifdef`/`#elif`/`#else`/`#endif` and
    !! fypp `#:if`/`#:elif`/`#:else`/`#:endif`) are treated as alternatives: each branch starts
    !! from the structure at the `#if`, and the structure at the end of the first branch carries
    !! on after the `#endif`. That keeps code such as `#ifdef X` / `do` / `#endif` ... `#ifdef X` /
    !! `end do` / `#endif` balanced.
    character(len=*), intent(in) :: src                           !! source text
    type(token_list_t), intent(in) :: tokens                      !! tokens of `src`
    type(statement_list_t), intent(in) :: stmts                   !! statements and other items of `src`
    type(stmt_info_t), allocatable, intent(out) :: info(:)        !! classification of each item (`ROLE_NONE` for non-code)
    integer, allocatable, intent(out) :: depth(:)                 !! nesting depth of each item
    type(diagnostic_t), allocatable, intent(out) :: diags(:)      !! structure problems found, in the order found
    integer, allocatable, intent(out), optional :: units(:)       !! for each item: enclosing program units
    integer, allocatable, intent(out), optional :: procedures(:)  !! for each item: enclosing procedure definitions

    type(scope_t), allocatable :: stack(:)
    type(saved_stack_t), allocatable :: saved(:)
    integer, allocatable :: nunits(:), nprocs(:)
    integer :: k, top, line, nsaved
    logical :: in_interface

    allocate (info(stmts%n), depth(stmts%n), diags(0), stack(16), saved(0))
    allocate (nunits(stmts%n), nprocs(stmts%n))
    top = 0
    nsaved = 0
    do k = 1, stmts%n
        depth(k) = top
        if (stmts%s(k)%kind == SK_PREPROC) call preprocessor(k)
        if (stmts%s(k)%kind /= SK_CODE) then
            call count_units(k)
            cycle
        end if
        line = tokens%tok(stmts%s(k)%first)%line
        in_interface = .false.
        if (top > 0) in_interface = stack(top)%construct == C_INTERFACE
        info(k) = classify_statement(src, tokens, stmts%s(k), in_interface)

        select case (info(k)%role)
        case (ROLE_OPEN)
            call push(scope_t(info(k)%construct, info(k)%do_label, line))
        case (ROLE_MIDDLE)
            if (top == 0 .and. info(k)%construct == C_CONTAINS) then
                ! a main program without a `program` statement
                call push(scope_t(C_PROGRAM, 0, line))
            else if (top == 0) then
                call diagnose(line, construct_label(info(k)%construct)//' statement outside any construct')
            else if (.not. middle_fits(info(k)%construct, stack(top)%construct)) then
                call diagnose(line, construct_label(info(k)%construct)//' statement inside '// &
                              construct_label(stack(top)%construct)//' (opened on line '//to_s(stack(top)%line)//')')
            else
                depth(k) = top - 1
            end if
        case (ROLE_CLOSE)
            call close_scope(info(k)%construct, line)
            depth(k) = top
        end select
        call count_units(k)

        ! the labeled statement that ends `do 10 ...` loops is part of the innermost loop body
        if (info(k)%label > 0) then
            do while (top > 0)
                if (stack(top)%construct /= C_DO .or. stack(top)%do_label /= info(k)%label) exit
                top = top - 1
            end do
        end if
    end do

    do k = top, 1, -1
        call diagnose(stack(k)%line, construct_label(stack(k)%construct)//' is not closed')
    end do
    if (present(units)) call move_alloc(nunits, units)
    if (present(procedures)) call move_alloc(nprocs, procedures)

contains

    subroutine push(s)
        !! Push scope `s` onto `stack`, growing it if it is full.
        type(scope_t), intent(in) :: s              !! the scope to push
        type(scope_t), allocatable :: tmp(:)
        if (top == size(stack)) then
            allocate (tmp(2*top))
            tmp(1:top) = stack(1:top)
            call move_alloc(tmp, stack)
        end if
        top = top + 1
        stack(top) = s
    end subroutine push

    subroutine count_units(k)
        !! Record how many of the scopes enclosing item `k` are program units, and procedures.
        integer, intent(in) :: k   !! index into `stmts`
        integer :: j
        nunits(k) = 0
        nprocs(k) = 0
        do j = 1, depth(k)
            select case (stack(j)%construct)
            case (C_MODULE, C_SUBMODULE, C_PROGRAM)
                nunits(k) = nunits(k) + 1
            case (C_SUBROUTINE, C_FUNCTION, C_PROCEDURE)
                ! interface bodies are declarations, not procedure definitions
                if (j == 1) then
                    nprocs(k) = nprocs(k) + 1
                else if (stack(j - 1)%construct /= C_INTERFACE) then
                    nprocs(k) = nprocs(k) + 1
                end if
            end select
        end do
    end subroutine count_units

    subroutine preprocessor(k)
        !! Save or restore the scope stack at the preprocessor conditionals in item `k`, and set
        !! the depth of the item to that of the restored stack.
        integer, intent(in) :: k   !! index into `stmts` of a preprocessor item
        integer :: j
        do j = stmts%s(k)%first, stmts%s(k)%last
            if (tokens%tok(j)%kind /= TK_PREPROC) cycle
            select case (conditional_kind(token_lower(src, tokens%tok(j))))
            case (PP_IF)
                nsaved = nsaved + 1
                if (nsaved > size(saved)) saved = [saved, saved_stack_t()]
                saved(nsaved)%at_if = stack(1:top)
                if (allocated(saved(nsaved)%after_first)) deallocate (saved(nsaved)%after_first)
            case (PP_ELSE)
                if (nsaved > 0) then
                    if (.not. allocated(saved(nsaved)%after_first)) saved(nsaved)%after_first = stack(1:top)
                    call restore(saved(nsaved)%at_if)
                    depth(k) = top
                end if
            case (PP_ENDIF)
                if (nsaved > 0) then
                    if (allocated(saved(nsaved)%after_first)) call restore(saved(nsaved)%after_first)
                    depth(k) = top
                    nsaved = nsaved - 1
                end if
            end select
        end do
    end subroutine preprocessor

    subroutine restore(scopes)
        !! Replace the scope stack by `scopes` (outermost first).
        type(scope_t), intent(in) :: scopes(:)      !! the saved stack
        integer :: j
        top = 0
        do j = 1, size(scopes)
            call push(scopes(j))
        end do
    end subroutine restore

    subroutine close_scope(construct, line)
        !! Pop the construct closed by an `end` statement, with a diagnostic if it does not match
        !! the innermost open construct. If an outer one matches, everything down to it is closed.
        integer, intent(in) :: construct   !! construct named by the `end` (`C_ANY` for a bare `end`)
        integer, intent(in) :: line        !! source line of the `end` statement
        integer :: j

        if (top == 0) then
            ! `end` or `end program` may close a main program without a `program` statement
            if (construct /= C_ANY .and. construct /= C_PROGRAM) &
                call diagnose(line, 'end '//construct_label(construct)//' without an open construct')
            return
        end if
        if (closes(construct, stack(top)%construct)) then
            top = top - 1
            return
        end if
        ! recover from a missing `end`: close down to a matching construct if there is one
        do j = top - 1, 1, -1
            if (closes(construct, stack(j)%construct)) then
                call diagnose(line, 'end '//construct_label(construct)//' closes '// &
                              construct_label(stack(top)%construct)//' (opened on line '//to_s(stack(top)%line)//')')
                top = j - 1
                return
            end if
        end do
        call diagnose(line, 'end '//construct_label(construct)//' does not match '// &
                      construct_label(stack(top)%construct)//' (opened on line '//to_s(stack(top)%line)//')')
    end subroutine close_scope

    subroutine diagnose(line, message)
        !! Add a diagnostic to `diags`.
        integer, intent(in) :: line             !! source line
        character(len=*), intent(in) :: message !! text of the warning
        diags = [diags, diagnostic_t(line, message)]
    end subroutine diagnose

end subroutine analyze_structure

pure integer function conditional_kind(text) result(kind)
    !! Kind of a preprocessor line (lowercase text): `PP_IF`, `PP_ELSE` (`#elif`, `#else`), `PP_ENDIF`
    !! or `PP_OTHER`.
    character(len=*), intent(in) :: text   !! text of the preprocessor token, in lowercase
    character(len=:), allocatable :: directive
    integer :: i, j

    kind = PP_OTHER
    i = 2                                   ! after '#'
    if (len(text) < 2 .or. text(1:1) /= '#') return
    if (text(2:2) == ':') i = 3             ! fypp
    do while (i <= len(text))
        if (text(i:i) /= ' ' .and. text(i:i) /= achar(9)) exit
        i = i + 1
    end do
    j = i
    do while (j <= len(text))
        if (text(j:j) < 'a' .or. text(j:j) > 'z') exit
        j = j + 1
    end do
    directive = text(i:j - 1)
    select case (directive)
    case ('if', 'ifdef', 'ifndef')
        kind = PP_IF
    case ('elif', 'else')
        kind = PP_ELSE
    case ('endif')
        kind = PP_ENDIF
    end select
end function conditional_kind

pure logical function closes(closing, open)
    !! True if an `end` of kind `closing` ends the open construct `open`.
    integer, intent(in) :: closing   !! construct named by the `end` (`C_ANY` for a bare `end`)
    integer, intent(in) :: open      !! construct that is open
    closes = closing == open .or. closing == C_ANY
    ! `end function` / `end subroutine` may close a separate module procedure, and vice versa
    if (closing == C_PROCEDURE) closes = closes .or. open == C_FUNCTION .or. open == C_SUBROUTINE
    if (open == C_PROCEDURE) closes = closes .or. closing == C_FUNCTION .or. closing == C_SUBROUTINE
end function closes

pure logical function middle_fits(middle, open)
    !! True if a middle statement of construct `middle` (`else` of `C_IF`, `contains`, ...) may
    !! appear directly inside the open construct `open`.
    integer, intent(in) :: middle   !! construct of the middle statement
    integer, intent(in) :: open     !! construct that is open
    select case (middle)
    case (C_CONTAINS)
        select case (open)
        case (C_PROGRAM, C_MODULE, C_SUBMODULE, C_SUBROUTINE, C_FUNCTION, C_PROCEDURE, C_TYPE)
            middle_fits = .true.
        case default
            middle_fits = .false.
        end select
    case default
        middle_fits = middle == open
    end select
end function middle_fits

pure function to_s(i) result(s)
    !! `i` as text, without blanks.
    integer, intent(in) :: i   !! the number
    character(len=:), allocatable :: s
    character(len=16) :: buf
    write (buf, '(i0)') i
    s = trim(buf)
end function to_s

end module ffmt_structure
