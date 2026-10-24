module ffmt_tokens
!! Tokens of free-form Fortran source.
!!
!! Every byte of the source belongs to exactly one token, including blanks, comments, line breaks
!! and continuation markers, so joining the text of all tokens reproduces the source exactly.
use ffmt_strings, only: make_lowercase
implicit none
private

public :: token_text, token_lower, is_code_kind, is_chevron, detokenize

! token kinds
integer, parameter, public :: TK_SPACE = 1      !! blanks and tabs
integer, parameter, public :: TK_NEWLINE = 2    !! LF, CRLF or CR
integer, parameter, public :: TK_COMMENT = 3    !! from `!` to the end of the line
integer, parameter, public :: TK_CONT = 4       !! continuation `&`
integer, parameter, public :: TK_PREPROC = 5    !! a cpp or fypp directive line, with its continuation lines
integer, parameter, public :: TK_NAME = 6       !! names and keywords
integer, parameter, public :: TK_INT = 7        !! integer literal, possibly with a kind (`10_ik`)
integer, parameter, public :: TK_REAL = 8       !! real literal (`1.0`, `.5e-3_dp`)
integer, parameter, public :: TK_STRING = 9     !! character literal, possibly with a kind prefix
integer, parameter, public :: TK_BOZ = 10       !! `b'0101'`, `o'17'`, `z'ff'`
integer, parameter, public :: TK_DOTOP = 11     !! `.and.`, `.eq.`, `.true.`, user-defined `.op.`
integer, parameter, public :: TK_OP = 12        !! operators and punctuation: `** // == /= <= >= => :: = + - * / < > ( ) [ ] , : ; %`
integer, parameter, public :: TK_FYPP = 13      !! inline fypp: `${...}$`, `@{...}@`, `#{...}#`
integer, parameter, public :: TK_OTHER = 14     !! any other character
integer, parameter, public :: TK_BOM = 15       !! UTF-8 byte order mark at the start of the file

! comment sub-kinds
integer, parameter, public :: CM_PLAIN = 0        !! ordinary comment
integer, parameter, public :: CM_DIRECTIVE = 1    !! compiler directive: `!$omp`, `!$acc`, `!dir$`, `!gcc$`, ...
integer, parameter, public :: CM_CONDITIONAL = 2  !! OpenMP conditional compilation: `!$ code`
integer, parameter, public :: CM_FORMATTER = 3    !! formatter directive: `!&`, `!&<`, `!&>`

! continuation sub-kinds
integer, parameter, public :: CT_TRAILING = 1     !! `&` ending a line
integer, parameter, public :: CT_LEADING = 2      !! `&` starting a continuation line

! string sub-kinds (bit flags)
integer, parameter, public :: STR_UNTERMINATED = 1  !! no closing quote before the end of the line
integer, parameter, public :: STR_MULTILINE = 2     !! continued across lines
integer, parameter, public :: STR_RAW_CONT = 4      !! a continuation line without a leading `&`, whose blanks are part of the string

type, public :: token_t
    !! One token: a byte range of the source with its kind.
    integer :: kind = 0       !! one of the TK_* constants
    integer :: first = 1      !! first byte in the source
    integer :: last = 0       !! last byte in the source
    integer :: line = 1       !! line of the first byte
    integer :: sub = 0        !! sub-kind (CM_*, CT_*, STR_*)
    logical :: glued = .false. !! joined to the previous code token across a line break, with no blank allowed between
end type token_t

type, public :: token_list_t
    !! The tokens of a source text, in source order.
    type(token_t), allocatable :: tok(:)   !! storage; only `tok(1:n)` is in use
    integer :: n = 0                       !! number of tokens
contains
    procedure :: push                      !! append a token
end type token_list_t

contains

subroutine push(self, t)
    !! Append token `t`, growing the storage as needed.
    class(token_list_t), intent(inout) :: self   !! the token list
    type(token_t), intent(in) :: t               !! token to append
    type(token_t), allocatable :: tmp(:)

    if (.not. allocated(self%tok)) allocate (self%tok(256))
    if (self%n == size(self%tok)) then
        allocate (tmp(2*self%n))
        tmp(1:self%n) = self%tok(1:self%n)
        call move_alloc(tmp, self%tok)
    end if
    self%n = self%n + 1
    self%tok(self%n) = t
end subroutine push

pure function token_text(src, t) result(text)
    !! Token text, as it appears in the source.
    character(len=*), intent(in) :: src   !! the source the token was read from
    type(token_t), intent(in) :: t        !! a token of `src`
    character(len=:), allocatable :: text
    text = src(t%first:t%last)
end function token_text

pure function token_lower(src, t) result(text)
    !! Token text in lowercase (ASCII).
    character(len=*), intent(in) :: src   !! the source the token was read from
    type(token_t), intent(in) :: t        !! a token of `src`
    character(len=:), allocatable :: text
    text = src(t%first:t%last)
    call make_lowercase(text)
end function token_lower

elemental logical function is_code_kind(kind)
    !! True for tokens that are part of the code, as opposed to layout (blanks, line breaks,
    !! continuation markers), comments and preprocessor lines.
    integer, intent(in) :: kind   !! token kind, one of the TK_* constants
    select case (kind)
    case (TK_SPACE, TK_NEWLINE, TK_COMMENT, TK_CONT, TK_PREPROC, TK_BOM)
        is_code_kind = .false.
    case default
        is_code_kind = .true.
    end select
end function is_code_kind

pure logical function is_chevron(src, tokens, t)
    !! True if token `t` is a `<` or `>` operator directly next to another one, as in the CUDA
    !! kernel launch `call k<<<grid, block>>>(x)`, where they are not relational operators.
    character(len=*), intent(in) :: src          !! the source the tokens were read from
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    integer, intent(in) :: t                     !! index into `tokens`
    is_chevron = .false.
    if (.not. is_angle(t)) return
    if (t > 1) is_chevron = is_angle(t - 1)
    if (t < tokens%n) is_chevron = is_chevron .or. is_angle(t + 1)

contains

    pure logical function is_angle(k)
        !! True if token `k` is a `<` or `>` operator.
        integer, intent(in) :: k   !! index into `tokens`
        is_angle = .false.
        if (tokens%tok(k)%kind /= TK_OP) return
        is_angle = src(tokens%tok(k)%first:tokens%tok(k)%last) == '<' .or. &
                   src(tokens%tok(k)%first:tokens%tok(k)%last) == '>'
    end function is_angle

end function is_chevron

function detokenize(src, tokens) result(text)
    !! Join the text of all tokens.
    character(len=*), intent(in) :: src          !! the source the tokens were read from
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    character(len=:), allocatable :: text
    integer :: k, n, total

    total = 0
    do k = 1, tokens%n
        total = total + tokens%tok(k)%last - tokens%tok(k)%first + 1
    end do
    allocate (character(len=total) :: text)
    n = 0
    do k = 1, tokens%n
        associate (t => tokens%tok(k))
            text(n + 1:n + t%last - t%first + 1) = src(t%first:t%last)
            n = n + t%last - t%first + 1
        end associate
    end do
end function detokenize

end module ffmt_tokens
