module ffmt_case
!! Letter case of keywords and dot operators.
!!
!! Fortran names are not reserved, so a word is only treated as a keyword where its position makes it
!! one: the leading words of a statement (`end subroutine`, `integer`, `if`, `else if`, `select case`,
!! `implicit none`, ...), the attributes of a declaration before `::`, `in`/`out`/`inout` in `intent()`,
!! `kind=`/`len=` in a type, `then`, `only:`, `result()` and `bind()`. A word that names something
!! (`call open(...)`, `subroutine real`, `end function do`) keeps its case. Changing the case of a name
!! never changes the meaning of the code, so a misclassified word is a matter of style only.
use ffmt_tokens
use ffmt_statements
use ffmt_options, only: case_options_t, CASE_KEEP
use ffmt_source, only: source_t
implicit none
private

public :: compute_case

! words that are keywords at the start of a statement (or of a declaration attribute)
character(len=*), parameter :: KEYWORDS(*) = [character(len=16) :: &
                                              'abstract', 'allocatable', 'allocate', 'assign', 'associate', &
                                              'asynchronous', 'backspace', 'bind', 'block', 'call', 'case', &
                                              'change', 'character', 'class', 'close', 'codimension', &
                                              'common', 'complex', 'concurrent', 'contains', 'contiguous', &
                                              'continue', 'critical', 'cycle', 'data', 'deallocate', &
                                              'default', 'deferred', 'dimension', 'do', 'double', &
                                              'doubleprecision', 'elemental', 'else', 'elseif', 'elsewhere', &
                                              'end', 'endfile', 'entry', 'enum', 'enumerator', &
                                              'equivalence', 'error', 'exit', 'extends', 'external', &
                                              'final', 'flush', 'forall', 'function', 'generic', 'go', &
                                              'goto', 'if', 'implicit', 'import', 'impure', 'include', &
                                              'inquire', 'integer', 'intent', 'interface', 'intrinsic', &
                                              'is', 'logical', 'module', 'namelist', 'non_intrinsic', &
                                              'non_overridable', 'non_recursive', 'none', 'nopass', &
                                              'nullify', 'open', 'optional', 'parameter', 'pass', 'pause', &
                                              'pointer', 'precision', 'print', 'private', 'procedure', &
                                              'program', 'protected', 'public', 'pure', 'rank', 'read', &
                                              'real', 'recursive', 'return', 'rewind', 'save', 'select', &
                                              'selectcase', 'selectrank', 'selecttype', 'sequence', &
                                              'simple', 'stop', 'submodule', 'subroutine', 'sync', 'target', &
                                              'team', 'then', 'type', 'use', 'value', 'volatile', 'wait', &
                                              'where', 'while', 'write']

! keywords followed by a name: the leading words of a statement end after them
character(len=*), parameter :: INTRODUCERS(*) = [character(len=12) :: &
                                                 'call', 'subroutine', 'function', 'program', 'module', 'submodule', 'use', &
                                                 'entry', &
                                                 'procedure', 'interface', 'go', 'goto', 'cycle', 'exit', 'data', 'common', &
                                                 'namelist', 'is']

contains

function compute_case(source, views, opts) result(mode)
    !! The case mode for each token (`CASE_KEEP`, `CASE_LOWER` or `CASE_UPPER`).
    type(source_t), intent(in) :: source            !! the source
    type(statement_view_t), intent(in) :: views(:)  !! the view of each item (see [[statement_views]])
    type(case_options_t), intent(in) :: opts        !! the case wanted for keywords and operators
    integer, allocatable :: mode(:)
    integer :: k, t

    allocate (mode(source%tokens%n))
    mode = CASE_KEEP
    if (opts%operators /= CASE_KEEP) then
        do t = 1, source%tokens%n
            if (source%tokens%tok(t)%kind == TK_DOTOP) mode(t) = opts%operators
        end do
    end if
    if (opts%keywords == CASE_KEEP) return
    do k = 1, source%stmts%n
        if (source%stmts%s(k)%kind == SK_CODE) call statement_case(views(k), opts%keywords, mode)
    end do
end function compute_case

subroutine statement_case(v, kw_mode, mode)
    !! Mark the keywords of one code statement with `kw_mode`.
    type(statement_view_t), intent(in) :: v    !! the statement
    integer, intent(in) :: kw_mode             !! case mode for keywords
    integer, intent(inout) :: mode(:)          !! case mode of each token, set for the keywords
    integer :: k

    if (v%n == 0) return
    k = v%first
    if (k + 1 < v%n) then
        if (v%kind_of(k) == TK_NAME .and. v%is(k + 1, ':')) k = k + 2   ! construct name
    end if
    if (v%is(k, 'format')) return
    call leading(k)
    call attributes(k)
    call trailing(k)

contains

    subroutine attributes(k)
        !! Mark the attributes of a declaration: the word after each top-level `,` before `::`, and
        !! `in`, `out` and `inout` in `intent(...)`.
        integer, intent(in) :: k   !! index into `v%code(:)` of the first word of the statement
        integer :: j
        if (.not. any([(v%is(j, '::') .and. v%depth(j) == 0, j=1, v%n)])) return
        do j = k, v%n
            if (v%depth(j) > 0) cycle
            if (v%is(j, '::')) exit
            if (v%is(j, ',') .and. is_keyword(j + 1)) then
                call mark(j + 1)
                if (v%is(j + 1, 'intent')) call intent_args(j + 2)
            end if
        end do
    end subroutine attributes

    subroutine trailing(k)
        !! Mark the keywords after the leading ones: `only` of a `use` statement, and `result` and
        !! `bind` after a parenthesis.
        integer, intent(in) :: k   !! index into `v%code(:)` of the first word of the statement
        integer :: j
        do j = k + 1, v%n
            if (v%depth(j) > 0) cycle
            select case (v%word(j))
            case ('only')
                if (v%is(j + 1, ':') .and. v%is(j - 1, ',')) call mark(j)
            case ('result', 'bind')
                if (v%is(j - 1, ')') .and. v%is(j + 1, '(')) call mark(j)
            end select
        end do
    end subroutine trailing

    subroutine mark(j)
        !! Give code token `j` the keyword case mode, if it is inside the statement.
        integer, intent(in) :: j   !! index into `v%code(:)`
        if (j >= 1 .and. j <= v%n) mode(v%code(j)) = kw_mode
    end subroutine mark

    logical function is_keyword(j)
        !! True if code token `j` is a name spelled like a keyword (including `endif`, `enddo`, ...).
        integer, intent(in) :: j   !! index into `v%code(:)`
        character(len=:), allocatable :: w
        is_keyword = .false.
        if (v%kind_of(j) /= TK_NAME) return
        w = v%word(j)
        if (any(KEYWORDS == w)) then
            is_keyword = .true.
        else if (len(w) > 3) then
            if (w(1:3) == 'end') is_keyword = any(KEYWORDS == w(4:)) .or. w(4:) == 'blockdata'
        end if
        if (w == 'blockdata') is_keyword = .true.
    end function is_keyword

    integer function after_parens(j) result(next)
        !! Index after the parenthesis group that opens at `j`, or `j` if there is none there.
        integer, intent(in) :: j   !! index into `v%code(:)`
        next = j
        if (.not. v%is(j, '(')) return
        next = v%closing(j)
        if (next == 0) next = v%n
        next = next + 1
    end function after_parens

    logical function is_assignment(j)
        !! True if the statement starting at `j` assigns to a variable: `name [(...)] =`, `=>` or `%`.
        integer, intent(in) :: j   !! index into `v%code(:)` of the first word
        integer :: m
        m = after_parens(j + 1)
        is_assignment = v%is(m, '=') .or. v%is(m, '=>') .or. v%is(m, '%') .or. &
                        v%is(j + 1, '=') .or. v%is(j + 1, '=>') .or. v%is(j + 1, '%')
    end function is_assignment

    subroutine intent_args(j)
        !! `in`, `out` and `inout` in `intent(...)` starting at the `(` at `j`.
        integer, intent(in) :: j   !! index into `v%code(:)`
        integer :: m
        if (.not. v%is(j, '(')) return
        do m = j + 1, v%n
            select case (v%word(m))
            case ('in', 'out', 'inout')
                call mark(m)
            case (')')
                return
            end select
        end do
    end subroutine intent_args

    subroutine type_params(j)
        !! `kind=` and `len=` in a type selector starting at the `(` at `j`.
        integer, intent(in) :: j   !! index into `v%code(:)`
        integer :: m, last
        last = v%closing(j)
        if (last == 0) then
            if (.not. v%is(j, '(')) return
            last = v%n
        end if
        do m = j + 1, last - 1
            if (v%depth(m) /= v%depth(j) + 1) cycle      ! directly inside the parentheses
            if ((v%is(m, 'kind') .or. v%is(m, 'len')) .and. v%is(m + 1, '=')) call mark(m)
        end do
    end subroutine type_params

    recursive subroutine leading(start)
        !! Mark the leading keywords of the statement starting at code token `start`.
        integer, intent(in) :: start   !! index into `v%code(:)`
        integer :: j, m
        character(len=:), allocatable :: w

        if (v%kind_of(start) /= TK_NAME) return
        if (is_assignment(start)) return
        j = start
        do while (j <= v%n)
            if (.not. is_keyword(j)) return
            w = v%word(j)
            call mark(j)
            if (w == 'go' .and. v%is(j + 1, 'to')) call mark(j + 1)
            if (any(INTRODUCERS == w)) return
            select case (w)
            case ('if', 'elseif', 'where', 'forall', 'while', 'case', 'rank')
                ! a condition or selector in parentheses, then `then` or another statement
                m = after_parens(j + 1)
                if (m == j + 1) then
                    j = j + 1
                    cycle
                end if
                if (v%is(m, 'then')) then
                    call mark(m)
                else if (w == 'if' .or. w == 'where' .or. w == 'forall') then
                    call leading(m)                  ! one-line `if (x) call f()`
                end if
                return
            case ('integer', 'real', 'complex', 'logical', 'character', 'type', 'class', 'precision', 'procedure')
                call type_params(j + 1)
                m = after_parens(j + 1)
                if (v%is(m, '*')) m = after_parens(m + 1) + merge(0, 1, v%is(m + 1, '('))
                j = m
                ! after a type, only a procedure prefix or `function` can follow; a name is declared
                select case (v%word(j))
                case ('function', 'pure', 'impure', 'elemental', 'recursive', 'non_recursive', 'module', 'simple')
                    continue
                case default
                    return
                end select
            case ('intent')
                call intent_args(j + 1)
                return
            case ('select')
                j = j + 1
            case default
                if (v%is(j + 1, '(')) return       ! `write(...)`, `allocate(...)`, `dimension(...)`
                j = j + 1
            end select
        end do
    end subroutine leading

end subroutine statement_case

end module ffmt_case
