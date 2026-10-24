module ffmt_structure
!! Block structure of a file: the nesting depth of every statement, from a stack of open
!! constructs, plus diagnostics for statements that do not fit (an `end` with nothing to close,
!! an `else` outside an `if`, a construct still open at the end of the file, ...).
use ffmt_tokens
use ffmt_statements
use ffmt_constructs
use ffmt_source, only: source_t
use ffmt_strings, only: to_s
implicit none
private

public :: analyze_structure

type, public :: diagnostic_t
    !! A problem with the block structure, such as an `end` that matches no open construct.
    integer :: line = 0                       !! source line of the statement concerned
    character(len=:), allocatable :: message  !! what is wrong, e.g. `'if is not closed'`
end type diagnostic_t

type, public :: structure_t
    !! The block structure of a source: for each item (see [[ffmt_statements]]) what it is and how
    !! deeply it is nested.
    type(stmt_info_t), allocatable :: info(:)   !! classification of each item (`ROLE_NONE` for non-code)
    integer, allocatable :: depth(:)            !! nesting depth of each item: the constructs around it
    integer, allocatable :: units(:)            !! how many of those are program units (module, submodule
                                                !! or main program)
    integer, allocatable :: procedures(:)       !! how many are procedure definitions (subroutine,
                                                !! function or separate module procedure, but not
                                                !! interface bodies)
    integer, allocatable :: fypp(:)             !! fypp blocks (`#:if`, `#:for`, `#:def`, ...) around each
                                                !! item; a directive line is outside its own block
end type structure_t

type :: scope_t
    !! An open construct on the scope stack.
    integer :: construct = C_NONE   !! one of the `C_*` constants
    integer :: do_label = 0         !! for `do 10 ...`: the label of the statement that ends the loop, else 0
    integer :: line = 0             !! source line of the opening statement
end type scope_t

type :: scope_stack_t
    !! The open constructs, outermost first.
    type(scope_t), allocatable :: s(:)   !! storage; only `s(1:top)` is in use
    integer :: top = 0                   !! number of open constructs
end type scope_stack_t

type :: saved_stack_t
    !! Scope stacks for one preprocessor conditional: as it was at the `#if` (restored at each
    !! `#elif` / `#else`) and as it was at the end of the first branch (used after `#endif`).
    type(scope_t), allocatable :: at_if(:)        !! the stack at the `#if`
    type(scope_t), allocatable :: after_first(:)  !! the stack at the end of the first branch; unallocated until then
end type saved_stack_t

! preprocessor conditionals
integer, parameter :: PP_OTHER = 0, PP_IF = 1, PP_ELSE = 2, PP_ENDIF = 3

! fypp block directives
integer, parameter :: FY_OTHER = 0   !! not a block directive (`#:set`, `$:`, cpp, ...)
integer, parameter :: FY_OPEN = 1    !! `#:if`, `#:for`, `#:def`, `#:block`, `#:call`, `#:mute`
integer, parameter :: FY_MIDDLE = 2  !! `#:elif`, `#:else`, `#:nextarg`
integer, parameter :: FY_CLOSE = 3   !! `#:endif`, `#:endfor`, ...

contains

subroutine analyze_structure(source, views, st, diags)
    !! Classify every statement and compute its depth: the number of constructs enclosing it, and
    !! how many of those are program units and procedures, so callers can choose not to indent
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
    type(source_t), intent(in) :: source                          !! the source
    type(statement_view_t), intent(in) :: views(:)                !! the view of each item (see [[statement_views]])
    type(structure_t), intent(out) :: st                          !! the structure
    type(diagnostic_t), allocatable, intent(out) :: diags(:)      !! structure problems found, in the order found

    type(scope_stack_t) :: stack
    type(saved_stack_t), allocatable :: saved(:)
    type(stmt_info_t), allocatable :: info(:)
    integer, allocatable :: depth(:), nunits(:), nprocs(:)
    integer :: k, line, nsaved
    logical :: in_interface

    associate (n => source%stmts%n)
        allocate (info(n), depth(n), nunits(n), nprocs(n), diags(0), saved(0))
    end associate
    nsaved = 0
    do k = 1, source%stmts%n
        depth(k) = stack%top
        if (source%stmts%s(k)%kind == SK_PREPROC) call preprocessor(k)
        if (source%stmts%s(k)%kind /= SK_CODE) then
            call count_units(k)
            cycle
        end if
        line = source%tokens%tok(source%stmts%s(k)%first)%line
        in_interface = .false.
        if (stack%top > 0) in_interface = stack%s(stack%top)%construct == C_INTERFACE
        info(k) = classify_statement(source%text, source%tokens, views(k), in_interface)

        select case (info(k)%role)
        case (ROLE_OPEN)
            call stack_push(stack, scope_t(info(k)%construct, info(k)%do_label, line))
        case (ROLE_MIDDLE)
            if (stack%top == 0 .and. info(k)%construct == C_CONTAINS) then
                ! a main program without a `program` statement
                call stack_push(stack, scope_t(C_PROGRAM, 0, line))
            else if (stack%top == 0) then
                call diagnose(line, construct_label(info(k)%construct)//' statement outside any construct')
            else if (.not. middle_fits(info(k)%construct, stack%s(stack%top)%construct)) then
                call diagnose(line, construct_label(info(k)%construct)//' statement inside '// &
                              opened(stack%s(stack%top)))
            else
                depth(k) = stack%top - 1
            end if
        case (ROLE_CLOSE)
            call close_scope(info(k)%construct, line)
            depth(k) = stack%top
        end select
        call count_units(k)

        ! the labeled statement that ends `do 10 ...` loops is part of the innermost loop body
        if (info(k)%label > 0) then
            do while (stack%top > 0)
                if (stack%s(stack%top)%construct /= C_DO .or. stack%s(stack%top)%do_label /= info(k)%label) exit
                stack%top = stack%top - 1
            end do
        end if
    end do

    do k = stack%top, 1, -1
        call diagnose(stack%s(k)%line, construct_label(stack%s(k)%construct)//' is not closed')
    end do
    call move_alloc(info, st%info)
    call move_alloc(depth, st%depth)
    call move_alloc(nunits, st%units)
    call move_alloc(nprocs, st%procedures)
    st%fypp = fypp_depths(source)

contains

    subroutine count_units(k)
        !! Record how many of the scopes enclosing item `k` are program units, and procedures.
        integer, intent(in) :: k   !! index into `stmts`
        integer :: j
        nunits(k) = 0
        nprocs(k) = 0
        do j = 1, depth(k)
            select case (stack%s(j)%construct)
            case (C_MODULE, C_SUBMODULE, C_PROGRAM)
                nunits(k) = nunits(k) + 1
            case (C_SUBROUTINE, C_FUNCTION, C_PROCEDURE)
                ! interface bodies are declarations, not procedure definitions
                if (j == 1) then
                    nprocs(k) = nprocs(k) + 1
                else if (stack%s(j - 1)%construct /= C_INTERFACE) then
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
        do j = source%stmts%s(k)%first, source%stmts%s(k)%last
            if (source%tokens%tok(j)%kind /= TK_PREPROC) cycle
            select case (conditional_kind(token_lower(source%text, source%tokens%tok(j))))
            case (PP_IF)
                nsaved = nsaved + 1
                if (nsaved > size(saved)) saved = [saved, saved_stack_t()]
                saved(nsaved)%at_if = stack_snapshot(stack)
                if (allocated(saved(nsaved)%after_first)) deallocate (saved(nsaved)%after_first)
            case (PP_ELSE)
                if (nsaved > 0) then
                    if (.not. allocated(saved(nsaved)%after_first)) saved(nsaved)%after_first = stack_snapshot(stack)
                    call stack_restore(stack, saved(nsaved)%at_if)
                    depth(k) = stack%top
                end if
            case (PP_ENDIF)
                if (nsaved > 0) then
                    if (allocated(saved(nsaved)%after_first)) call stack_restore(stack, saved(nsaved)%after_first)
                    depth(k) = stack%top
                    nsaved = nsaved - 1
                end if
            end select
        end do
    end subroutine preprocessor

    subroutine close_scope(construct, line)
        !! Pop the construct closed by an `end` statement, with a diagnostic if it does not match
        !! the innermost open construct. If an outer one matches, everything down to it is closed.
        integer, intent(in) :: construct   !! construct named by the `end` (`C_ANY` for a bare `end`)
        integer, intent(in) :: line        !! source line of the `end` statement
        integer :: j

        if (stack%top == 0) then
            ! `end` or `end program` may close a main program without a `program` statement
            if (construct /= C_ANY .and. construct /= C_PROGRAM) &
                call diagnose(line, 'end '//construct_label(construct)//' without an open construct')
            return
        end if
        if (closes(construct, stack%s(stack%top)%construct)) then
            stack%top = stack%top - 1
            return
        end if
        ! recover from a missing `end`: close down to a matching construct if there is one
        do j = stack%top - 1, 1, -1
            if (closes(construct, stack%s(j)%construct)) then
                call diagnose(line, 'end '//construct_label(construct)//' closes '// &
                              opened(stack%s(stack%top)))
                stack%top = j - 1
                return
            end if
        end do
        call diagnose(line, 'end '//construct_label(construct)//' does not match '// &
                      opened(stack%s(stack%top)))
    end subroutine close_scope

    function opened(sc) result(text)
        !! `sc` for messages: `do (opened on line 12)`.
        type(scope_t), intent(in) :: sc   !! an open construct
        character(len=:), allocatable :: text
        text = construct_label(sc%construct)//' (opened on line '//to_s(sc%line)//')'
    end function opened

    subroutine diagnose(line, message)
        !! Add a diagnostic to `diags`.
        integer, intent(in) :: line             !! source line
        character(len=*), intent(in) :: message !! text of the warning
        diags = [diags, diagnostic_t(line, message)]
    end subroutine diagnose

end subroutine analyze_structure

pure subroutine stack_push(self, sc)
    !! Open construct `sc`, growing the storage as needed.
    type(scope_stack_t), intent(inout) :: self   !! the stack
    type(scope_t), intent(in) :: sc               !! the construct to open
    type(scope_t), allocatable :: tmp(:)
    if (.not. allocated(self%s)) allocate (self%s(16))
    if (self%top == size(self%s)) then
        allocate (tmp(2*self%top))
        tmp(1:self%top) = self%s(1:self%top)
        call move_alloc(tmp, self%s)
    end if
    self%top = self%top + 1
    self%s(self%top) = sc
end subroutine stack_push

pure subroutine stack_restore(self, scopes)
    !! Replace the open constructs by `scopes` (outermost first).
    type(scope_stack_t), intent(inout) :: self   !! the stack
    type(scope_t), intent(in) :: scopes(:)        !! a saved stack (see [[stack_snapshot]])
    integer :: j
    self%top = 0
    do j = 1, size(scopes)
        call stack_push(self, scopes(j))
    end do
end subroutine stack_restore

pure function stack_snapshot(self) result(scopes)
    !! The open constructs, outermost first.
    type(scope_stack_t), intent(in) :: self   !! the stack
    type(scope_t), allocatable :: scopes(:)
    if (allocated(self%s)) then
        scopes = self%s(1:self%top)
    else
        allocate (scopes(0))
    end if
end function stack_snapshot

function fypp_depths(source) result(fypp)
    !! The number of fypp blocks around each item of `source`. The directives are counted wherever
    !! they are, also inside a continued statement; a directive line itself is at the level outside
    !! its block, like `else` and `end` statements.
    type(source_t), intent(in) :: source   !! the source
    integer, allocatable :: fypp(:)
    integer :: k, j, d

    allocate (fypp(source%stmts%n))
    d = 0
    do k = 1, source%stmts%n
        fypp(k) = d
        do j = source%stmts%s(k)%first, source%stmts%s(k)%last
            if (source%tokens%tok(j)%kind /= TK_PREPROC) cycle
            select case (fypp_kind(token_lower(source%text, source%tokens%tok(j))))
            case (FY_OPEN)
                d = d + 1
            case (FY_MIDDLE)
                if (source%stmts%s(k)%kind == SK_PREPROC) fypp(k) = max(0, d - 1)
            case (FY_CLOSE)
                d = max(0, d - 1)
                if (source%stmts%s(k)%kind == SK_PREPROC) fypp(k) = d
            end select
        end do
    end do
end function fypp_depths

pure integer function fypp_kind(text) result(kind)
    !! Kind of a preprocessor line (lowercase text) as a fypp block directive: one of the FY_*
    !! constants.
    character(len=*), intent(in) :: text   !! text of the preprocessor token, in lowercase
    integer :: i, j

    kind = FY_OTHER
    if (len(text) < 3) return
    if (text(1:2) /= '#:') return
    i = 3
    do while (i <= len(text))
        if (text(i:i) /= ' ' .and. text(i:i) /= achar(9)) exit
        i = i + 1
    end do
    j = i
    do while (j <= len(text))
        if (text(j:j) < 'a' .or. text(j:j) > 'z') exit
        j = j + 1
    end do
    select case (text(i:j - 1))
    case ('if', 'for', 'def', 'block', 'call', 'mute')
        kind = FY_OPEN
    case ('elif', 'else', 'nextarg')
        kind = FY_MIDDLE
    case ('endif', 'endfor', 'enddef', 'endblock', 'endcall', 'endmute')
        kind = FY_CLOSE
    end select
end function fypp_kind

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

end module ffmt_structure
