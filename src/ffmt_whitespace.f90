module ffmt_whitespace
!! Whitespace rules within lines: the blanks wanted between pairs of adjacent tokens.
!!
!! Each rule has a mode: `WS_ADD` makes sure there is at least one blank on each side (more are
!! kept, so column alignment survives), `WS_REMOVE` removes the blanks, and `WS_KEEP` leaves them as
!! they are. Rules only look at tokens on the same line; blanks are never added or removed next to a
!! line break, a continuation `&` or a comment.
!!
!! Statements where operators have other meanings are recognized: unary `+`/`-`, `*` in `real*8`,
!! `print *` and `(*)`, `/` in array constructors, and in `data`, `common`, `namelist`, `save` and
!! `equivalence` statements. `format` statements are left alone.
use ffmt_tokens
use ffmt_statements
implicit none
private

public :: compute_gaps

! rule modes
integer, parameter, public :: WS_KEEP = 0     !! leave the blanks as they are
integer, parameter, public :: WS_ADD = 1      !! at least one blank on each side
integer, parameter, public :: WS_REMOVE = 2   !! no blanks

! gaps: the blanks wanted before a token
integer, parameter, public :: GAP_KEEP = -1   !! as in the input
integer, parameter, public :: GAP_MIN1 = -2   !! at least one blank

type, public :: whitespace_options_t
    !! Modes of the whitespace rules. The defaults follow the most common style in practice where it is
    !! clear-cut, and keep the rest.
    integer :: assignment = WS_ADD     !! `=` outside brackets: assignments, initializations, `do i = 1, n`
    integer :: keyword_args = WS_REMOVE !! `=` inside brackets: `f(x=1)`, `intent=`, `len=*`
    integer :: pointer = WS_ADD        !! `=>`: pointer assignment, renames, `associate`
    integer :: declarations = WS_ADD   !! `::`
    integer :: relational = WS_KEEP    !! `== /= < <= > >=` and `.eq.` etc.
    integer :: logical = WS_KEEP       !! `.and. .or. .eqv. .neqv.`
    integer :: plus_minus = WS_KEEP    !! binary `+ -`
    integer :: mult_div = WS_KEEP      !! binary `* /`
    integer :: power = WS_REMOVE       !! `**`
    integer :: concat = WS_REMOVE      !! `//`
    integer :: comma = WS_KEEP         !! `,`: with `WS_ADD`, no blank before and at least one after
    integer :: slice_colon = WS_REMOVE !! `:` inside brackets: `a(1:n)`
    integer :: percent = WS_REMOVE     !! `%`
    integer :: control_paren = WS_ADD  !! between `if`, `while`, `select case`, `where`, ... and `(`, and
                                       !! between `)` and a final `then`
end type whitespace_options_t

contains

function compute_gaps(src, tokens, stmts, ws) result(gap)
    !! The gap wanted before each token (`GAP_KEEP`, `GAP_MIN1` or a number of blanks).
    character(len=*), intent(in) :: src                 !! the source text
    type(token_list_t), intent(in) :: tokens            !! its tokens
    type(statement_list_t), intent(in) :: stmts         !! its statements
    type(whitespace_options_t), intent(in) :: ws        !! modes of the whitespace rules
    integer, allocatable :: gap(:)
    integer :: k

    allocate (gap(tokens%n))
    gap = GAP_KEEP
    do k = 1, stmts%n
        if (stmts%s(k)%kind == SK_CODE) call statement_gaps(src, tokens, stmts%s(k), ws, gap)
    end do
end function compute_gaps

subroutine statement_gaps(src, tokens, stmt, ws, gap)
    !! Set the gaps wanted before the tokens of one code statement.
    character(len=*), intent(in) :: src                 !! the source text
    type(token_list_t), intent(in) :: tokens            !! its tokens
    type(statement_t), intent(in) :: stmt               !! the statement, a code statement
    type(whitespace_options_t), intent(in) :: ws        !! modes of the whitespace rules
    integer, intent(inout) :: gap(:)                    !! gap wanted before each token, set where a rule applies

    integer, allocatable :: code(:)
    integer :: i, n, depth, first
    character(len=:), allocatable :: kw
    logical :: slash_lists

    code = code_tokens(src, tokens, stmt)
    n = size(code)
    if (n == 0) return
    first = 1
    if (tokens%tok(code(1))%kind == TK_INT .and. n > 1) first = 2      ! statement label
    kw = text(first)
    if (kw == 'format') return
    ! `data x /1/`, `common /blk/ a`, ...: slashes are delimiters, not division
    slash_lists = kw == 'data' .or. kw == 'common' .or. kw == 'namelist' .or. kw == 'save' .or. &
                  kw == 'equivalence'

    depth = 0
    do i = 1, n
        if (tokens%tok(code(i))%kind == TK_OP) then
            ! operators have no letters, so their text is used as it is, without a lowercase copy
            associate (t => src(tokens%tok(code(i))%first:tokens%tok(code(i))%last))
                select case (t)
                case ('(', '[')
                    if (t == '(' .and. is_control_paren(i)) call set_before(i, ws%control_paren)
                    depth = depth + 1
                case (')', ']')
                    depth = max(0, depth - 1)
                case ('=')
                    if (depth == 0) then
                        call around(i, ws%assignment)
                    else
                        call around(i, ws%keyword_args)
                    end if
                case ('=>')
                    call around(i, ws%pointer)
                case ('::')
                    call around(i, ws%declarations)
                case ('==', '/=', '<=', '>=')
                    call around(i, ws%relational)
                case ('<', '>')
                    if (.not. chevron(i)) call around(i, ws%relational)
                case ('+', '-')
                    if (binary(i)) call around(i, ws%plus_minus)
                case ('*')
                    if (binary(i) .and. .not. type_star(i)) call around(i, ws%mult_div)
                case ('/')
                    if (binary(i) .and. .not. slash_lists) call around(i, ws%mult_div)
                case ('**')
                    if (binary(i)) call around(i, ws%power)
                case ('//')
                    if (binary(i) .and. .not. slash_lists) call around(i, ws%concat)
                case ('%')
                    call around(i, ws%percent)
                case (':')
                    ! a slice `a(1 : n, :)`: only between the colon and its bounds
                    if (depth > 0) then
                        if (ends_operand(i - 1)) call set_before(i, ws%slice_colon)
                        if (starts_operand(i + 1)) call set_before(i + 1, ws%slice_colon)
                    end if
                case (',')
                    select case (ws%comma)
                    case (WS_ADD)
                        call set_gap(i, 0)
                        call set_gap(i + 1, GAP_MIN1)
                    case (WS_REMOVE)
                        call set_gap(i, 0)
                        call set_gap(i + 1, 0)
                    end select
                end select
            end associate
        else if (tokens%tok(code(i))%kind == TK_NAME) then
            ! `if (x)then`: the blank before a final `then` goes with the one before the condition
            if (i == n) then
                if (text(i) == 'then' .and. text(i - 1) == ')') call set_before(i, ws%control_paren)
            end if
        else if (tokens%tok(code(i))%kind == TK_DOTOP) then
            select case (text(i))
            case ('.eq.', '.ne.', '.lt.', '.le.', '.gt.', '.ge.')
                call around(i, ws%relational)
            case ('.and.', '.or.', '.eqv.', '.neqv.')
                call around(i, ws%logical)
            end select
        end if
    end do

contains

    function text(j) result(s)
        !! Lowercase text of code token `j`, or '' outside the statement.
        integer, intent(in) :: j   !! index into `code(:)`
        character(len=:), allocatable :: s
        if (j < 1 .or. j > n) then
            s = ''
        else
            s = token_lower(src, tokens%tok(code(j)))
        end if
    end function text

    logical function adjacent(j)
        !! True if code tokens `j - 1` and `j` are on the same line with only blanks between them.
        integer, intent(in) :: j   !! index into `code(:)`
        integer :: k
        adjacent = .false.
        if (j < 2 .or. j > n) return
        if (tokens%tok(code(j))%glued) return
        do k = code(j - 1) + 1, code(j) - 1
            if (tokens%tok(k)%kind /= TK_SPACE) return
        end do
        adjacent = .true.
    end function adjacent

    subroutine set_gap(j, g)
        !! Ask for gap `g` before code token `j`, if it is on the same line as the previous token and the
        !! two tokens can be written next to each other without becoming different tokens.
        integer, intent(in) :: j   !! index into `code(:)`
        integer, intent(in) :: g   !! the gap: `GAP_MIN1` or a number of blanks
        if (.not. adjacent(j)) return
        if (g == 0 .and. .not. can_join(j)) return
        gap(code(j)) = g
    end subroutine set_gap

    subroutine set_before(j, mode)
        !! Apply rule mode `mode` to the blanks before code token `j`.
        integer, intent(in) :: j      !! index into `code(:)`
        integer, intent(in) :: mode   !! `WS_KEEP`, `WS_ADD` or `WS_REMOVE`
        select case (mode)
        case (WS_ADD)
            call set_gap(j, GAP_MIN1)
        case (WS_REMOVE)
            call set_gap(j, 0)
        end select
    end subroutine set_before

    subroutine around(j, mode)
        !! Apply `mode` on both sides of operator `j`.
        integer, intent(in) :: j      !! index into `code(:)`
        integer, intent(in) :: mode   !! `WS_KEEP`, `WS_ADD` or `WS_REMOVE`
        call set_before(j, mode)
        call set_before(j + 1, mode)
    end subroutine around

    logical function can_join(j)
        !! True if tokens `j - 1` and `j` keep their meaning when written without a blank between them.
        integer, intent(in) :: j   !! index into `code(:)`
        character(len=:), allocatable :: a, b
        integer :: ka, kb
        a = text(j - 1)
        b = text(j)
        ka = tokens%tok(code(j - 1))%kind
        kb = tokens%tok(code(j))%kind
        can_join = .true.
        if (is_word(ka) .and. is_word(kb)) can_join = .false.           ! `a b`, `1 e5`
        if (ka == TK_OP .and. kb == TK_OP) then                          ! `* *` would become `**`
            select case (a//b)
            case ('**', '//', '==', '/=', '<=', '>=', '=>', '::', '(/', '/)')
                can_join = .false.
            end select
        end if
        if ((ka == TK_INT .or. ka == TK_REAL) .and. b(1:1) == '.') can_join = .false.   ! `1 .5`, `1. .eq.`
        if (ka == TK_DOTOP .and. kb == TK_DOTOP) can_join = .false.
    end function can_join

    logical function binary(j)
        !! True if operator `j` has an operand on both sides (not unary `-x` or `print *`).
        integer, intent(in) :: j   !! index into `code(:)`
        binary = ends_operand(j - 1) .and. starts_operand(j + 1)
    end function binary

    logical function ends_operand(j)
        !! True if code token `j` can end an operand: a name, literal, inline fypp, logical constant,
        !! `)` or `]`, but not a statement label.
        integer, intent(in) :: j   !! index into `code(:)`
        ends_operand = .false.
        if (j < 1) return
        select case (tokens%tok(code(j))%kind)
        case (TK_NAME, TK_INT, TK_REAL, TK_STRING, TK_BOZ, TK_FYPP)
            ends_operand = j > first .or. tokens%tok(code(j))%kind /= TK_INT   ! not a statement label
        case (TK_DOTOP)
            ends_operand = text(j) == '.true.' .or. text(j) == '.false.' .or. index(text(j), '._') > 0
        case (TK_OP)
            ends_operand = text(j) == ')' .or. text(j) == ']'
        end select
    end function ends_operand

    logical function starts_operand(j)
        !! True if code token `j` can start an operand: a name, literal, inline fypp, dot operator,
        !! `(`, `[` or a sign.
        integer, intent(in) :: j   !! index into `code(:)`
        starts_operand = .false.
        if (j > n) return
        select case (tokens%tok(code(j))%kind)
        case (TK_NAME, TK_INT, TK_REAL, TK_STRING, TK_BOZ, TK_FYPP, TK_DOTOP)
            starts_operand = .true.
        case (TK_OP)
            starts_operand = text(j) == '(' .or. text(j) == '[' .or. text(j) == '+' .or. text(j) == '-'
        end select
    end function starts_operand

    logical function type_star(j)
        !! True for the `*` of a length or kind selector: `real*8`, `character*(*)`, `character*10`.
        integer, intent(in) :: j   !! index into `code(:)`
        select case (text(j - 1))
        case ('real', 'integer', 'complex', 'logical', 'character', 'precision')
            type_star = .true.
        case default
            type_star = .false.
        end select
    end function type_star

    logical function chevron(j)
        !! True for `<` or `>` next to another one: CUDA kernel launches `<<< ... >>>`.
        integer, intent(in) :: j   !! index into `code(:)`
        chevron = .false.
        if (j > 1) chevron = (text(j - 1) == '<' .or. text(j - 1) == '>') .and. adjacent_raw(j)
        if (j < n) chevron = chevron .or. ((text(j + 1) == '<' .or. text(j + 1) == '>') .and. adjacent_raw(j + 1))
    end function chevron

    logical function adjacent_raw(j)
        !! True if code tokens `j - 1` and `j` touch.
        integer, intent(in) :: j   !! index into `code(:)`
        adjacent_raw = code(j) == code(j - 1) + 1
    end function adjacent_raw

    logical function is_control_paren(j)
        !! True if `(` at `j` follows a control keyword: `if (`, `else if (`, `do while (`,
        !! `select case (`, `where (`, `forall (`, `case (`, `rank (`, `do concurrent (`.
        integer, intent(in) :: j   !! index into `code(:)`
        character(len=:), allocatable :: prev
        is_control_paren = .false.
        if (j < 2) return
        prev = text(j - 1)
        select case (prev)
        case ('if', 'elseif')
            is_control_paren = j - 1 == first .or. text(j - 2) == 'else' .or. text(j - 2) == ':'
        case ('while', 'concurrent')
            is_control_paren = text(j - 2) == 'do'
        case ('case', 'type', 'rank')
            is_control_paren = text(j - 2) == 'select' .or. (prev /= 'type' .and. j - 1 == first)
        case ('selectcase', 'selecttype', 'selectrank', 'where', 'forall')
            is_control_paren = j - 1 == first .or. text(j - 2) == ':'
        end select
    end function is_control_paren

end subroutine statement_gaps

elemental logical function is_word(kind)
    !! Tokens that would run together with a following word: names, numbers and BOZ literals.
    integer, intent(in) :: kind   !! token kind (`TK_*`)
    is_word = kind == TK_NAME .or. kind == TK_INT .or. kind == TK_REAL .or. kind == TK_BOZ
end function is_word

end module ffmt_whitespace
