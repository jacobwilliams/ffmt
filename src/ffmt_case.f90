module ffmt_case
!! Letter case of keywords, dot operators, intrinsic procedures, the constants of the intrinsic
!! modules, and kinds.
!!
!! Fortran names are not reserved, so a word is only treated as a keyword where its position makes it
!! one: the leading words of a statement (`end subroutine`, `integer`, `if`, `else if`, `select case`,
!! `implicit none`, ...), the attributes of a declaration before `::`, `in`/`out`/`inout` in `intent()`,
!! `kind=`/`len=` in a type, `then`, `only:`, `result()` and `bind()`. A word that names something
!! (`call open(...)`, `subroutine real`, `end function do`) keeps its case. Changing the case of a name
!! never changes the meaning of the code, so a misclassified word is a matter of style only.
!!
!! Intrinsic procedures are recognized where they are called (followed by `(`, or after `call`), and the
!! constants and types of `iso_fortran_env` and `iso_c_binding` wherever they are used, but not as the
!! first word of a statement, after `%`, as an entity being declared, or as the name of a procedure
!! being defined. Kinds are the values of `kind=`, the kind of a declaration's type (`real(dp) :: x`),
!! the kind of a literal (`2_int64`, `.true._lk`, `ck_'a'`) and exponent letters (`1.0e-3`).
use ffmt_tokens
use ffmt_statements
use ffmt_options, only: case_options_t, CASE_KEEP
use ffmt_source, only: source_t
implicit none
private

public :: compute_case, kind_part

type, public :: case_marks_t
    !! The letter case wanted for each token (`CASE_KEEP`, `CASE_LOWER` or `CASE_UPPER`).
    integer, allocatable :: whole(:)   !! for the whole text of each token
    integer, allocatable :: part(:)    !! for the kind part of a token only (see [[kind_part]]), applied
                                       !! after `whole`
end type case_marks_t

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
                                              'enumeration', 'equivalence', 'error', 'exit', 'extends', 'external', &
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

! intrinsic procedures (F2023 table 16.1), specific names, and the procedures of the intrinsic modules
character(len=*), parameter :: INTRINSIC_PROCEDURES(*) = [character(len=24) :: &
                                                          'abs', 'achar', 'acos', 'acosd', 'acosh', 'acospi', 'adjustl', &
                                                          'adjustr', 'aimag', &
                                                          'aint', 'all', 'allocated', 'alog', 'alog10', 'amax0', 'amax1', 'amin0', &
                                                          'amin1', 'amod', &
                                                          'anint', 'any', 'asin', 'asind', 'asinh', 'asinpi', 'associated', &
                                                          'atan', 'atan2', &
                                                          'atan2d', 'atan2pi', 'atand', 'atanh', 'atanpi', 'atomic_add', &
                                                          'atomic_and', &
                                                          'atomic_cas', 'atomic_define', 'atomic_fetch_add', 'atomic_fetch_and', &
                                                          'atomic_fetch_or', &
                                                          'atomic_fetch_xor', 'atomic_or', 'atomic_ref', 'atomic_xor', &
                                                          'bessel_j0', 'bessel_j1', &
                                                          'bessel_jn', 'bessel_y0', 'bessel_y1', 'bessel_yn', 'bge', 'bgt', &
                                                          'bit_size', 'ble', &
                                                          'blt', 'btest', 'c_associated', 'c_f_pointer', 'c_f_procpointer', &
                                                          'c_f_strpointer', &
                                                          'c_funloc', 'c_loc', 'c_sizeof', 'cabs', 'ccos', 'ceiling', 'cexp', &
                                                          'char', 'clog', &
                                                          'cmplx', 'co_broadcast', 'co_max', 'co_min', 'co_reduce', 'co_sum', &
                                                          'command_argument_count', 'compiler_options', 'compiler_version', &
                                                          'conjg', 'cos', 'cosd', &
                                                          'cosh', 'coshape', 'cospi', 'count', 'cpu_time', 'cshift', 'csin', &
                                                          'csqrt', 'dabs', &
                                                          'dacos', 'dasin', 'datan', 'datan2', 'date_and_time', 'dble', 'dcos', &
                                                          'dcosh', 'ddim', &
                                                          'dexp', 'digits', 'dim', 'dint', 'dlog', 'dlog10', 'dmax1', 'dmin1', &
                                                          'dmod', 'dnint', &
                                                          'dot_product', 'dprod', 'dshiftl', 'dshiftr', 'dsign', 'dsin', 'dsinh', &
                                                          'dsqrt', 'dtan', &
                                                          'dtanh', 'eoshift', 'epsilon', 'erf', 'erfc', 'erfc_scaled', &
                                                          'event_query', &
                                                          'execute_command_line', 'exp', 'exponent', 'extends_type_of', &
                                                          'f_c_string', &
                                                          'failed_images', 'findloc', 'float', 'floor', 'fraction', 'gamma', &
                                                          'get_command', &
                                                          'get_command_argument', 'get_environment_variable', 'get_team', 'huge', &
                                                          'hypot', 'iabs', &
                                                          'iachar', 'iall', 'iand', 'iany', 'ibclr', 'ibits', 'ibset', 'ichar', &
                                                          'idim', 'idint', &
                                                          'idnint', 'ieor', 'ifix', 'image_index', 'image_status', 'index', 'int', &
                                                          'ior', &
                                                          'iparity', 'is_contiguous', 'is_iostat_end', 'is_iostat_eor', 'ishft', &
                                                          'ishftc', 'isign', &
                                                          'kind', 'lbound', 'lcobound', 'leadz', 'len', 'len_trim', 'lge', 'lgt', &
                                                          'lle', 'llt', &
                                                          'log', 'log10', 'log_gamma', 'logical', 'maskl', 'maskr', 'matmul', &
                                                          'max', 'max0', &
                                                          'max1', 'maxexponent', 'maxloc', 'maxval', 'merge', 'merge_bits', 'min', &
                                                          'min0', 'min1', &
                                                          'minexponent', 'minloc', 'minval', 'mod', 'modulo', 'move_alloc', &
                                                          'mvbits', 'nearest', &
                                                          'new_line', 'nint', 'norm2', 'not', 'null', 'num_images', &
                                                          'out_of_range', 'pack', &
                                                          'parity', 'popcnt', 'poppar', 'precision', 'present', 'product', &
                                                          'radix', 'random_init', &
                                                          'random_number', 'random_seed', 'range', 'rank', 'real', 'reduce', &
                                                          'repeat', 'reshape', &
                                                          'rrspacing', 'same_type_as', 'scale', 'scan', 'selected_char_kind', &
                                                          'selected_int_kind', &
                                                          'selected_logical_kind', 'selected_real_kind', 'set_exponent', 'shape', &
                                                          'shifta', &
                                                          'shiftl', 'shiftr', 'sign', 'sin', 'sind', 'sinh', 'sinpi', 'size', &
                                                          'sngl', 'spacing', &
                                                          'split', 'spread', 'sqrt', 'stopped_images', 'storage_size', 'sum', &
                                                          'system_clock', &
                                                          'tan', 'tand', 'tanh', 'tanpi', 'team_number', 'this_image', 'tiny', &
                                                          'tokenize', &
                                                          'trailz', 'transfer', 'transpose', 'trim', 'ubound', 'ucobound', &
                                                          'unpack', 'verify']

! named constants and types of iso_fortran_env and iso_c_binding
character(len=*), parameter :: INTRINSIC_CONSTANTS(*) = [character(len=28) :: &
                                                         'atomic_int_kind', 'atomic_logical_kind', 'c_alert', 'c_backspace', &
                                                         'c_bool', &
                                                         'c_carriage_return', 'c_char', 'c_double', 'c_double_complex', 'c_float', &
                                                         'c_float128', &
                                                         'c_float128_complex', 'c_float_complex', 'c_form_feed', 'c_funptr', &
                                                         'c_horizontal_tab', &
                                                         'c_int', 'c_int128_t', 'c_int16_t', 'c_int32_t', 'c_int64_t', 'c_int8_t', &
                                                         'c_int_fast128_t', 'c_int_fast16_t', 'c_int_fast32_t', 'c_int_fast64_t', &
                                                         'c_int_fast8_t', &
                                                         'c_int_least128_t', 'c_int_least16_t', 'c_int_least32_t', &
                                                         'c_int_least64_t', &
                                                         'c_int_least8_t', 'c_intmax_t', 'c_intptr_t', 'c_long', 'c_long_double', &
                                                         'c_long_double_complex', 'c_long_long', 'c_new_line', 'c_null_char', &
                                                         'c_null_funptr', &
                                                         'c_null_ptr', 'c_ptr', 'c_ptrdiff_t', 'c_short', 'c_signed_char', &
                                                         'c_size_t', &
                                                         'c_vertical_tab', 'character_kinds', 'character_storage_size', &
                                                         'current_team', &
                                                         'error_unit', 'event_type', 'file_storage_size', 'initial_team', &
                                                         'input_unit', 'int16', &
                                                         'int32', 'int64', 'int8', 'integer_kinds', 'iostat_end', 'iostat_eor', &
                                                         'iostat_inquire_internal_unit', 'lock_type', 'logical16', 'logical32', &
                                                         'logical64', &
                                                         'logical8', 'logical_kinds', 'notify_type', 'numeric_storage_size', &
                                                         'output_unit', &
                                                         'parent_team', 'real128', 'real16', 'real32', 'real64', 'real_kinds', &
                                                         'stat_failed_image', 'stat_locked', 'stat_locked_other_image', &
                                                         'stat_stopped_image', &
                                                         'stat_unlocked', 'stat_unlocked_failed_image', 'team_type']

contains

function compute_case(source, views, opts) result(marks)
    !! The letter case wanted for each token.
    type(source_t), intent(in) :: source            !! the source
    type(statement_view_t), intent(in) :: views(:)  !! the view of each item (see [[statement_views]])
    type(case_options_t), intent(in) :: opts        !! the case wanted for each category
    type(case_marks_t) :: marks
    integer :: k, t, a, b

    allocate (marks%whole(source%tokens%n), marks%part(source%tokens%n))
    marks%whole = CASE_KEEP
    marks%part = CASE_KEEP
    do t = 1, source%tokens%n
        associate (tok => source%tokens%tok(t))
            select case (tok%kind)
            case (TK_DOTOP)
                marks%whole(t) = opts%operators
            case (TK_INT, TK_REAL)
                marks%whole(t) = opts%types          ! their only letters: an exponent and a kind
            end select
            if (opts%types /= CASE_KEEP .and. (tok%kind == TK_DOTOP .or. tok%kind == TK_STRING)) then
                call kind_part(source%text(tok%first:tok%last), tok%kind, a, b)
                if (b >= a) marks%part(t) = opts%types
            end if
        end associate
    end do
    do k = 1, source%stmts%n
        if (source%stmts%s(k)%kind /= SK_CODE) cycle
        if (opts%keywords /= CASE_KEEP) call statement_case(views(k), opts%keywords, marks%whole)
        if (opts%procedures /= CASE_KEEP .or. opts%constants /= CASE_KEEP .or. opts%types /= CASE_KEEP) &
            call names_case(views(k), opts, marks%whole)
    end do
end function compute_case

pure subroutine kind_part(text, kind, a, b)
    !! Where the kind is in the text of a token of kind `kind`: `text(a:b)`, or `b < a` if it has none.
    !! For a logical constant it is the suffix (`_lk` in `.true._lk`), for a string the prefix (`ck` in
    !! `ck_'a'`). Numbers have letters only in their exponent and kind, so they are cased whole.
    character(len=*), intent(in) :: text   !! the token's text
    integer, intent(in) :: kind            !! its kind (`TK_*`)
    integer, intent(out) :: a, b           !! the range of the kind in `text`
    integer :: p
    a = 1
    b = 0
    select case (kind)
    case (TK_DOTOP)
        p = index(text, '.', back=.true.)
        if (p > 1 .and. p < len(text)) then
            if (text(p + 1:p + 1) == '_') then
                a = p + 2
                b = len(text)
            end if
        end if
    case (TK_STRING)
        p = scan(text, '"'//"'")
        if (p > 2) then
            if (text(p - 1:p - 1) == '_') b = p - 2
        end if
    end select
end subroutine kind_part

subroutine names_case(v, opts, mode)
    !! Mark the intrinsic procedures, the constants of the intrinsic modules and the kinds named in
    !! one code statement.
    type(statement_view_t), intent(in) :: v      !! the statement
    type(case_options_t), intent(in) :: opts     !! the case wanted for each category
    integer, intent(inout) :: mode(:)            !! case mode of each token, set for the names
    integer :: j, first
    logical :: decl, use_stmt, entity

    if (v%n == 0) return
    first = v%first
    if (first + 1 < v%n) then
        if (v%kind_of(first) == TK_NAME .and. v%is(first + 1, ':')) first = first + 2   ! construct name
    end if
    use_stmt = v%is(first, 'use')
    decl = .false.
    if (.not. use_stmt) decl = any([(v%is(j, '::') .and. v%depth(j) == 0, j=1, v%n)])
    entity = .false.             ! after `::`, the next top-level name is an entity being declared
    do j = first, v%n
        if (decl .and. v%depth(j) == 0) then
            if (v%is(j, '::') .or. (entity .and. v%is(j, ','))) then
                entity = .true.
                cycle
            end if
        end if
        if (v%kinds(j) /= TK_NAME) cycle
        if (j == first) cycle
        if (v%is(j - 1, '%')) cycle
        if (decl .and. entity .and. v%depth(j) == 0 .and. (v%is(j - 1, '::') .or. v%is(j - 1, ','))) cycle
        select case (v%word(j - 1))
        case ('function', 'subroutine', 'procedure', 'entry', 'program', 'module', 'submodule', 'interface')
            cycle
        end select
        if (opts%procedures /= CASE_KEEP .and. (v%is(j + 1, '(') .or. v%is(j - 1, 'call'))) then
            if (any(INTRINSIC_PROCEDURES == v%word(j))) mode(v%code(j)) = opts%procedures
        end if
        if (opts%constants /= CASE_KEEP .and. .not. v%is(j + 1, '=')) then
            if (any(INTRINSIC_CONSTANTS == v%word(j))) mode(v%code(j)) = opts%constants
        end if
        if (opts%types /= CASE_KEEP .and. is_kind(j)) mode(v%code(j)) = opts%types
    end do

contains

    logical function is_kind(j)
        !! True if name `j` is a kind: the value of `kind=`, or the kind of the type that starts a
        !! declaration (`real(dp) :: x`, `integer(int64), parameter :: n = 1`).
        integer, intent(in) :: j   !! index into `v%code(:)`
        is_kind = v%is(j - 1, '=') .and. v%is(j - 2, 'kind')
        if (is_kind .or. .not. decl) return
        if (j - 2 /= first .or. .not. v%is(j - 1, '(') .or. .not. v%is(j + 1, ')')) return
        select case (v%word(first))
        case ('integer', 'real', 'complex', 'logical', 'character')
            is_kind = .true.
        end select
    end function is_kind

end subroutine names_case

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
