module ffmt_relational
!! Spelling of the relational operators: `==` / `.eq.`, `/=` / `.ne.`, `<` / `.lt.`, `<=` / `.le.`,
!! `>` / `.gt.` and `>=` / `.ge.`. The two spellings are the same operator, also in `operator(...)`.
!!
!! CUDA kernel launches (`<<< ... >>>`) and code protected by formatter directives are not changed.
use ffmt_tokens
use ffmt_lexer, only: tokenize
use ffmt_statements
use ffmt_directives, only: protected_items
use ffmt_strings, only: string_builder_t
implicit none
private

public :: convert_relational, canonical_relational

integer, parameter, public :: REL_KEEP = 0      !! leave the spelling as it is
integer, parameter, public :: REL_SYMBOLS = 1   !! `.lt.` becomes `<`, ...
integer, parameter, public :: REL_LETTERS = 2   !! `<` becomes `.lt.`, ...

character(len=*), parameter :: SYMBOLS(6) = [character(len=2) :: '==', '/=', '<', '<=', '>', '>=']
character(len=*), parameter :: LETTERS(6) = [character(len=4) :: '.eq.', '.ne.', '.lt.', '.le.', '.gt.', '.ge.']

contains

function convert_relational(src, style) result(out)
    !! `src` with every relational operator written in `style` (`REL_SYMBOLS` or `REL_LETTERS`).
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: style          !! one of the REL_* constants; `REL_KEEP` returns `src`
    character(len=:), allocatable :: out

    type(token_list_t) :: tokens
    type(statement_list_t) :: stmts
    logical, allocatable :: keep(:)
    type(string_builder_t) :: buf
    integer :: k, j, r

    if (style == REL_KEEP) then
        out = src
        return
    end if
    call tokenize(src, tokens)
    call split_statements(src, tokens, stmts)
    keep = protected_items(src, tokens, stmts)
    do k = 1, stmts%n
        do j = stmts%s(k)%first, stmts%s(k)%last
            r = 0
            if (.not. keep(k) .and. stmts%s(k)%kind == SK_CODE) r = operator_index(j)
            if (r == 0) then
                call buf%append(token_text(src, tokens%tok(j)))
            else if (style == REL_SYMBOLS) then
                call buf%append(trim(SYMBOLS(r)))
            else
                call buf%append(trim(LETTERS(r)))
            end if
        end do
    end do
    out = buf%get()

contains

    integer function operator_index(j)
        !! Index of token `j` in the operator tables, or 0 if it is not a relational operator.
        integer, intent(in) :: j   !! index into the token list
        character(len=:), allocatable :: t
        operator_index = 0
        if (tokens%tok(j)%kind /= TK_OP .and. tokens%tok(j)%kind /= TK_DOTOP) return
        t = token_lower(src, tokens%tok(j))
        if (tokens%tok(j)%kind == TK_OP) then
            operator_index = findloc(SYMBOLS, t, dim=1)
            if ((t == '<' .or. t == '>') .and. chevron(j)) operator_index = 0
        else
            operator_index = findloc(LETTERS, t, dim=1)
        end if
    end function operator_index

    logical function chevron(j)
        !! True for `<` or `>` directly next to another one, as in `<<<` and `>>>`.
        integer, intent(in) :: j   !! index into the token list
        chevron = .false.
        if (j > 1) chevron = is_angle(j - 1)
        if (j < tokens%n) chevron = chevron .or. is_angle(j + 1)
    end function chevron

    logical function is_angle(j)
        !! True if token `j` is a `<` or `>` operator.
        integer, intent(in) :: j   !! index into the token list
        character(len=:), allocatable :: t
        is_angle = .false.
        if (tokens%tok(j)%kind /= TK_OP) return
        t = token_text(src, tokens%tok(j))
        is_angle = t == '<' .or. t == '>'
    end function is_angle

end function convert_relational

pure function canonical_relational(text) result(t)
    !! The symbol spelling of a lowercase relational operator; other text is returned unchanged.
    character(len=*), intent(in) :: text   !! operator text, in lowercase
    character(len=:), allocatable :: t
    integer :: r
    r = findloc(LETTERS, text, dim=1)
    if (r > 0) then
        t = trim(SYMBOLS(r))
    else
        t = text
    end if
end function canonical_relational

end module ffmt_relational
