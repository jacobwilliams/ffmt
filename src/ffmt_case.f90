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
implicit none
private

public :: compute_case

! case modes
integer, parameter, public :: CASE_KEEP = 0
integer, parameter, public :: CASE_LOWER = 1
integer, parameter, public :: CASE_UPPER = 2

type, public :: case_options_t
    !! Letter case of keywords and of dot operators and logical constants.
    integer :: keywords = CASE_KEEP    !! `if`, `end do`, `integer`, `intent(in)`, ...
    integer :: operators = CASE_KEEP   !! `.and.`, `.eq.`, `.true.`, ...
end type case_options_t

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

function compute_case(src, tokens, stmts, opts) result(mode)
    !! The case mode for each token (`CASE_KEEP`, `CASE_LOWER` or `CASE_UPPER`).
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_list_t), intent(in) :: stmts
    type(case_options_t), intent(in) :: opts
    integer, allocatable :: mode(:)
    integer :: k, t

    allocate (mode(tokens%n))
    mode = CASE_KEEP
    if (opts%operators /= CASE_KEEP) then
        do t = 1, tokens%n
            if (tokens%tok(t)%kind == TK_DOTOP) mode(t) = opts%operators
        end do
    end if
    if (opts%keywords == CASE_KEEP) return
    do k = 1, stmts%n
        if (stmts%s(k)%kind == SK_CODE) call statement_case(src, tokens, stmts%s(k), opts%keywords, mode)
    end do
end function compute_case

subroutine statement_case(src, tokens, stmt, kw_mode, mode)
    !! Mark the keywords of one statement with `kw_mode`.
    character(len=*), intent(in) :: src
    type(token_list_t), intent(in) :: tokens
    type(statement_t), intent(in) :: stmt
    integer, intent(in) :: kw_mode
    integer, intent(inout) :: mode(:)

    integer, allocatable :: code(:)
    integer :: n, k, j, depth
    logical :: decl

    code = code_tokens(src, tokens, stmt)
    n = size(code)
    if (n == 0) return
    k = 1
    if (kind_of(1) == TK_INT .and. n > 1) k = 2                    ! statement label
    if (k + 1 < n) then
        if (kind_of(k) == TK_NAME .and. word(k + 1) == ':') k = k + 2   ! construct name
    end if
    if (word(k) == 'format') return
    call leading(k)

    ! attributes of a declaration: the word after each top-level `,` before `::`
    decl = .false.
    depth = 0
    do j = 1, n
        select case (word(j))
        case ('(', '[')
            depth = depth + 1
        case (')', ']')
            depth = max(0, depth - 1)
        case ('::')
            if (depth == 0) decl = .true.
        end select
    end do
    if (decl) then
        depth = 0
        do j = k, n
            select case (word(j))
            case ('(', '[')
                depth = depth + 1
            case (')', ']')
                depth = max(0, depth - 1)
            case (',')
                if (depth == 0 .and. is_keyword(j + 1)) then
                    call mark(j + 1)
                    if (word(j + 1) == 'intent') call intent_args(j + 2)
                end if
            case ('::')
                if (depth == 0) exit
            end select
        end do
    end if

    ! `only:` of a `use` statement, `result(...)` and `bind(...)` after a parenthesis
    depth = 0
    do j = k + 1, n
        select case (word(j))
        case ('(', '[')
            depth = depth + 1
        case (')', ']')
            depth = max(0, depth - 1)
        case ('only')
            if (depth == 0 .and. word(j + 1) == ':' .and. word(j - 1) == ',') call mark(j)
        case ('result', 'bind')
            if (depth == 0 .and. word(j - 1) == ')' .and. word(j + 1) == '(') call mark(j)
        end select
    end do

contains

    function word(j) result(w)
        !! Lowercase text of code token `j`, or '' outside the statement.
        integer, intent(in) :: j
        character(len=:), allocatable :: w
        if (j < 1 .or. j > n) then
            w = ''
        else
            w = token_lower(src, tokens%tok(code(j)))
        end if
    end function word

    integer function kind_of(j)
        integer, intent(in) :: j
        kind_of = 0
        if (j >= 1 .and. j <= n) kind_of = tokens%tok(code(j))%kind
    end function kind_of

    subroutine mark(j)
        integer, intent(in) :: j
        if (j >= 1 .and. j <= n) mode(code(j)) = kw_mode
    end subroutine mark

    logical function is_keyword(j)
        !! True if code token `j` is a name spelled like a keyword (including `endif`, `enddo`, ...).
        integer, intent(in) :: j
        character(len=:), allocatable :: w
        is_keyword = .false.
        if (kind_of(j) /= TK_NAME) return
        w = word(j)
        if (any(KEYWORDS == w)) then
            is_keyword = .true.
        else if (len(w) > 3) then
            if (w(1:3) == 'end') is_keyword = any(KEYWORDS == w(4:)) .or. w(4:) == 'blockdata'
        end if
        if (w == 'blockdata') is_keyword = .true.
    end function is_keyword

    integer function after_parens(j) result(next)
        !! Index after the parenthesis group that opens at `j`, or `j` if there is none there.
        integer, intent(in) :: j
        integer :: d
        next = j
        if (word(j) /= '(') return
        d = 0
        do next = j, n
            if (word(next) == '(') d = d + 1
            if (word(next) == ')') then
                d = d - 1
                if (d == 0) exit
            end if
        end do
        next = next + 1
    end function after_parens

    logical function is_assignment(j)
        !! True if the statement starting at `j` assigns to a variable: `name [(...)] =`, `=>` or `%`.
        integer, intent(in) :: j
        integer :: m
        m = after_parens(j + 1)
        select case (word(m))
        case ('=', '=>', '%')
            is_assignment = .true.
        case default
            is_assignment = .false.
        end select
        if (word(j + 1) == '=' .or. word(j + 1) == '=>' .or. word(j + 1) == '%') is_assignment = .true.
    end function is_assignment

    subroutine intent_args(j)
        !! `in`, `out` and `inout` in `intent(...)` starting at the `(` at `j`.
        integer, intent(in) :: j
        integer :: m
        if (word(j) /= '(') return
        do m = j + 1, n
            select case (word(m))
            case ('in', 'out', 'inout')
                call mark(m)
            case (')')
                return
            end select
        end do
    end subroutine intent_args

    subroutine type_params(j)
        !! `kind=` and `len=` in a type selector starting at the `(` at `j`.
        integer, intent(in) :: j
        integer :: m, d
        if (word(j) /= '(') return
        d = 0
        do m = j, n
            select case (word(m))
            case ('(')
                d = d + 1
            case (')')
                d = d - 1
                if (d == 0) return
            case ('kind', 'len')
                if (d == 1 .and. word(m + 1) == '=') call mark(m)
            end select
        end do
    end subroutine type_params

    recursive subroutine leading(start)
        !! Mark the leading keywords of the statement starting at code token `start`.
        integer, intent(in) :: start
        integer :: j, m
        character(len=:), allocatable :: w

        if (kind_of(start) /= TK_NAME) return
        if (is_assignment(start)) return
        j = start
        do while (j <= n)
            if (.not. is_keyword(j)) return
            w = word(j)
            call mark(j)
            if (w == 'go' .and. word(j + 1) == 'to') call mark(j + 1)
            if (any(INTRODUCERS == w)) return
            select case (w)
            case ('if', 'elseif', 'where', 'forall', 'while', 'case', 'rank')
                ! a condition or selector in parentheses, then `then` or another statement
                m = after_parens(j + 1)
                if (m == j + 1) then
                    j = j + 1
                    cycle
                end if
                if (word(m) == 'then') then
                    call mark(m)
                else if (w == 'if' .or. w == 'where' .or. w == 'forall') then
                    call leading(m)                  ! one-line `if (x) call f()`
                end if
                return
            case ('integer', 'real', 'complex', 'logical', 'character', 'type', 'class', 'precision', 'procedure')
                call type_params(j + 1)
                m = after_parens(j + 1)
                if (word(m) == '*') m = after_parens(m + 1) + merge(0, 1, word(m + 1) == '(')
                j = m
                ! after a type, only a procedure prefix or `function` can follow; a name is declared
                select case (word(j))
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
                if (word(j + 1) == '(') return       ! `write(...)`, `allocate(...)`, `dimension(...)`
                j = j + 1
            end select
        end do
    end subroutine leading

end subroutine statement_case

end module ffmt_case
