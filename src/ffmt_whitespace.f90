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
use ffmt_options, only: whitespace_options_t, WS_ADD, WS_REMOVE
use ffmt_source, only: source_t
use ffmt_strings, only: utf8_width
implicit none
private

public :: compute_gaps

! gaps: the blanks wanted before a token
integer, parameter, public :: GAP_KEEP = -1   !! as in the input
integer, parameter, public :: GAP_MIN1 = -2   !! at least one blank

contains

function compute_gaps(source, views, ws, comment_spacing) result(gap)
    !! The gap wanted before each token (`GAP_KEEP`, `GAP_MIN1` or a number of blanks).
    type(source_t), intent(in) :: source                !! the source
    type(statement_view_t), intent(in) :: views(:)      !! the view of each item (see [[statement_views]])
    type(whitespace_options_t), intent(in) :: ws        !! modes of the whitespace rules
    integer, intent(in) :: comment_spacing              !! blanks before a trailing comment, or -1 to keep them
    integer, allocatable :: gap(:)
    integer :: k

    allocate (gap(source%tokens%n))
    gap = GAP_KEEP
    do k = 1, source%stmts%n
        if (source%stmts%s(k)%kind == SK_CODE) call statement_gaps(source%text, source%tokens, views(k), ws, gap)
    end do
    if (comment_spacing >= 0) call comment_gaps(source, comment_spacing, gap)
end function compute_gaps

subroutine comment_gaps(source, spacing, gap)
    !! Ask for `spacing` blanks before each trailing comment (one after code or a continuation `&`),
    !! unless a trailing comment on the line before or after starts in the same column: comments
    !! lined up in a column are left as they are.
    type(source_t), intent(in) :: source   !! the source
    integer, intent(in) :: spacing         !! blanks wanted before a trailing comment
    integer, intent(inout) :: gap(:)       !! gap wanted before each token, set for the comments
    integer, allocatable :: tok(:), line(:), col(:)
    integer :: t, k, n, p

    allocate (tok(source%tokens%n), line(source%tokens%n), col(source%tokens%n))
    n = 0
    do t = 2, source%tokens%n
        if (source%tokens%tok(t)%kind /= TK_COMMENT) cycle
        p = t - 1
        if (source%tokens%tok(p)%kind == TK_SPACE .and. p > 1) p = p - 1
        if (.not. (is_code_kind(source%tokens%tok(p)%kind) .or. source%tokens%tok(p)%kind == TK_CONT)) cycle
        if (source%tokens%tok(p)%kind == TK_SPACE) cycle
        n = n + 1
        tok(n) = t
        line(n) = source%tokens%tok(t)%line
        col(n) = column(source%tokens%tok(t)%first)
    end do
    do k = 1, n
        if (k > 1) then
            if (line(k - 1) == line(k) - 1 .and. col(k - 1) == col(k)) cycle
        end if
        if (k < n) then
            if (line(k + 1) == line(k) + 1 .and. col(k + 1) == col(k)) cycle
        end if
        gap(tok(k)) = spacing
    end do

contains

    integer function column(i)
        !! Input column of byte `i`: the characters before it on its line.
        integer, intent(in) :: i   !! index into the source text
        integer :: j
        j = scan(source%text(1:i - 1), achar(10)//achar(13), back=.true.)
        column = utf8_width(source%text(j + 1:i - 1))
    end function column

end subroutine comment_gaps

subroutine statement_gaps(src, tokens, v, ws, gap)
    !! Set the gaps wanted before the tokens of one code statement.
    character(len=*), intent(in) :: src                 !! the source text
    type(token_list_t), intent(in) :: tokens            !! its tokens
    type(statement_view_t), intent(in) :: v             !! the statement
    type(whitespace_options_t), intent(in) :: ws        !! modes of the whitespace rules
    integer, intent(inout) :: gap(:)                    !! gap wanted before each token, set where a rule applies

    integer :: i
    logical :: slash_lists

    if (v%n == 0) return
    if (v%is(v%first, 'format')) return
    ! `data x /1/`, `common /blk/ a`, ...: slashes are delimiters, not division
    select case (v%word(v%first))
    case ('data', 'common', 'namelist', 'save', 'equivalence')
        slash_lists = .true.
    case default
        slash_lists = .false.
    end select

    do i = 1, v%n
        if (v%kinds(i) == TK_OP) then
            associate (t => v%low(v%lo(i):v%hi(i)))
                select case (t)
                case ('(')
                    if (is_control_paren(i)) call set_before(i, ws%control_paren)
                case ('=')
                    if (v%depth(i) == 0) then
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
                    if (.not. is_chevron(src, tokens, v%code(i))) call around(i, ws%relational)
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
                case ('?')
                    call around(i, ws%conditional)
                case (':')
                    if (conditional_colon(i)) then
                        call around(i, ws%conditional)
                    else if (v%depth(i) > 0) then
                        ! a slice `a(1 : n, :)`: only between the colon and its bounds
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
        else if (v%kinds(i) == TK_NAME) then
            ! `if (x)then`: the blank before a final `then` goes with the one before the condition
            if (i == v%n .and. v%is(i, 'then') .and. v%is(i - 1, ')')) call set_before(i, ws%control_paren)
        else if (v%kinds(i) == TK_DOTOP) then
            select case (v%word(i))
            case ('.eq.', '.ne.', '.lt.', '.le.', '.gt.', '.ge.')
                call around(i, ws%relational)
            case ('.and.', '.or.', '.eqv.', '.neqv.')
                call around(i, ws%logical)
            case ('.not.', '.true.', '.false.')
                continue
            case default
                ! a user-defined operator, unless it is a logical constant with a kind (`.true._lk`)
                if (index(v%word(i), '._') == 0 .and. binary(i)) call around(i, ws%defined_operators)
            end select
        end if
    end do

contains

    logical function adjacent(j)
        !! True if code tokens `j - 1` and `j` are on the same line with only blanks between them.
        integer, intent(in) :: j   !! index into `code(:)`
        integer :: k
        adjacent = .false.
        if (j < 2 .or. j > v%n) return
        if (tokens%tok(v%code(j))%glued) return
        do k = v%code(j - 1) + 1, v%code(j) - 1
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
        gap(v%code(j)) = g
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
        integer :: ka, kb
        ka = v%kinds(j - 1)
        kb = v%kinds(j)
        can_join = .true.
        if (is_word(ka) .and. is_word(kb)) can_join = .false.           ! `a b`, `1 e5`
        if (ka == TK_OP .and. kb == TK_OP) then                          ! `* *` would become `**`
            select case (v%word(j - 1)//v%word(j))
            case ('**', '//', '==', '/=', '<=', '>=', '=>', '::', '(/', '/)')
                can_join = .false.
            end select
        end if
        if ((ka == TK_INT .or. ka == TK_REAL) .and. v%low(v%lo(j):v%lo(j)) == '.') can_join = .false.   ! `1 .5`
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
        select case (v%kind_of(j))
        case (TK_NAME, TK_INT, TK_REAL, TK_STRING, TK_BOZ, TK_FYPP)
            ends_operand = j > v%first .or. v%kinds(j) /= TK_INT   ! not a statement label
        case (TK_DOTOP)
            ends_operand = v%is(j, '.true.') .or. v%is(j, '.false.') .or. index(v%word(j), '._') > 0
        case (TK_OP)
            ends_operand = v%is(j, ')') .or. v%is(j, ']')
        end select
    end function ends_operand

    logical function starts_operand(j)
        !! True if code token `j` can start an operand: a name, literal, inline fypp, dot operator,
        !! `(`, `[` or a sign.
        integer, intent(in) :: j   !! index into `code(:)`
        starts_operand = .false.
        select case (v%kind_of(j))
        case (TK_NAME, TK_INT, TK_REAL, TK_STRING, TK_BOZ, TK_FYPP, TK_DOTOP)
            starts_operand = .true.
        case (TK_OP)
            starts_operand = v%is(j, '(') .or. v%is(j, '[') .or. v%is(j, '+') .or. v%is(j, '-')
        end select
    end function starts_operand

    logical function type_star(j)
        !! True for the `*` of a length or kind selector: `real*8`, `character*(*)`, `character*10`.
        integer, intent(in) :: j   !! index into `code(:)`
        select case (v%word(j - 1))
        case ('real', 'integer', 'complex', 'logical', 'character', 'precision')
            type_star = .true.
        case default
            type_star = .false.
        end select
    end function type_star

    logical function conditional_colon(j)
        !! True if the `:` at `j` belongs to a conditional expression: a `?` comes before it at the same
        !! bracket depth, inside the same brackets.
        integer, intent(in) :: j   !! index into `v%code(:)` of a `:`
        integer :: k
        conditional_colon = .false.
        if (v%depth(j) == 0) return
        do k = j - 1, 1, -1
            if (v%depth(k) < v%depth(j)) return          ! the opening bracket
            if (v%depth(k) == v%depth(j) .and. v%is(k, '?')) then
                conditional_colon = .true.
                return
            end if
        end do
    end function conditional_colon

    logical function is_control_paren(j)
        !! True if `(` at `j` follows a control keyword: `if (`, `else if (`, `do while (`,
        !! `select case (`, `where (`, `forall (`, `case (`, `rank (`, `do concurrent (`.
        integer, intent(in) :: j   !! index into `code(:)`
        is_control_paren = .false.
        if (j < 2) return
        select case (v%word(j - 1))
        case ('if', 'elseif')
            is_control_paren = j - 1 == v%first .or. v%is(j - 2, 'else') .or. v%is(j - 2, ':')
        case ('while', 'concurrent')
            is_control_paren = v%is(j - 2, 'do')
        case ('case', 'rank')
            is_control_paren = v%is(j - 2, 'select') .or. j - 1 == v%first
        case ('type')
            is_control_paren = v%is(j - 2, 'select')
        case ('selectcase', 'selecttype', 'selectrank', 'where', 'forall')
            is_control_paren = j - 1 == v%first .or. v%is(j - 2, ':')
        end select
    end function is_control_paren

end subroutine statement_gaps

elemental logical function is_word(kind)
    !! Tokens that would run together with a following word: names, numbers and BOZ literals.
    integer, intent(in) :: kind   !! token kind (`TK_*`)
    is_word = kind == TK_NAME .or. kind == TK_INT .or. kind == TK_REAL .or. kind == TK_BOZ
end function is_word

end module ffmt_whitespace
