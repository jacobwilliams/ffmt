module ffmt_lexer
!! Lexer for free-form Fortran.
!!
!! The lexer never fails: malformed input (e.g. an unterminated string) still produces tokens
!! that cover every byte, flagged where relevant, so the formatter can leave such code alone.
use ffmt_tokens
use ffmt_strings, only: lower_char, lowercase
implicit none
private

public :: tokenize

character, parameter :: LF = achar(10), CR = achar(13), TAB = achar(9)

type :: lexer_t
    !! Lexer state for one source text.
    integer :: line = 1               !! current line
    logical :: line_start = .true.    !! only blanks so far on the current line
    logical :: glue_pending = .false. !! the previous line ended with a code character directly followed by `&`
    logical :: glue_next = .false.    !! the next code token continues the previous one
end type lexer_t

contains

subroutine tokenize(src, tokens)
    !! Split `src` into tokens.
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(out) :: tokens    !! its tokens, covering every byte

    type(lexer_t) :: lx
    integer :: i, j, n, kind, sub
    logical :: at_start
    character :: c

    n = len(src)
    i = 1
    if (n >= 3) then
        if (src(1:3) == achar(239)//achar(187)//achar(191)) then
            call add(tokens, lx, src, TK_BOM, 1, 3)
            i = 4
        end if
    end if
    do while (i <= n)
        c = src(i:i)
        sub = 0

        if (c == LF .or. c == CR) then
            j = i
            if (c == CR .and. peek(src, i + 1) == LF) j = i + 1
            call add(tokens, lx, src, TK_NEWLINE, i, j)
            lx%line_start = .true.
            i = j + 1
            cycle
        end if
        if (c == ' ' .or. c == TAB) then
            j = i
            do while (is_blank(peek(src, j + 1)))
                j = j + 1
            end do
            call add(tokens, lx, src, TK_SPACE, i, j)
            i = j + 1
            cycle
        end if

        at_start = lx%line_start
        lx%line_start = .false.
        if (at_start .and. is_preproc_start(src, i)) then
            call add(tokens, lx, src, TK_PREPROC, i, preproc_end(src, i))
            i = tokens%tok(tokens%n)%last + 1
            cycle
        end if

        select case (c)
        case ('!')
            j = line_end(src, i)
            call add(tokens, lx, src, TK_COMMENT, i, j, comment_kind(src(i:j)))
        case ('&')
            j = i
            if (at_start) then
                sub = CT_LEADING
            else
                sub = CT_TRAILING
            end if
            call add(tokens, lx, src, TK_CONT, i, j, sub)
        case ("'", '"')
            j = string_end(src, i, sub)
            call add(tokens, lx, src, TK_STRING, i, j, sub)
        case ('0':'9')
            call lex_number(src, i, j, kind)
            if (peek(src, j + 1) == '_' .and. is_quote(peek(src, j + 2))) then
                ! kind parameter prefix of a string: 1_'abc'
                j = string_end(src, j + 2, sub)
                kind = TK_STRING
            end if
            call add(tokens, lx, src, kind, i, j, sub)
        case ('.')
            if (is_digit(peek(src, i + 1))) then
                call lex_number(src, i, j, kind)
                call add(tokens, lx, src, kind, i, j)
            else if (dotop_at(src, i)) then
                j = dotop_end(src, i)
                call add(tokens, lx, src, TK_DOTOP, i, j)
            else
                j = i
                call add(tokens, lx, src, TK_OTHER, i, j)
            end if
        case ('a':'z', 'A':'Z', '_')   ! a leading `_` is not standard, but appears in generated code
            j = name_end(src, i)
            kind = TK_NAME
            if (is_quote(peek(src, j + 1))) then
                if (j == i .and. index('bozBOZ', c) > 0) then
                    kind = TK_BOZ
                    j = string_end(src, j + 1, sub)
                else if (src(j:j) == '_') then
                    kind = TK_STRING
                    j = string_end(src, j + 1, sub)
                end if
            end if
            call add(tokens, lx, src, kind, i, j, sub)
        case default
            j = fypp_inline_end(src, i)
            if (j > 0) then
                call add(tokens, lx, src, TK_FYPP, i, j)
            else
                j = operator_end(src, i)
                if (j > 0) then
                    call add(tokens, lx, src, TK_OP, i, j)
                else
                    j = i
                    call add(tokens, lx, src, TK_OTHER, i, j)
                end if
            end if
        end select
        i = j + 1
    end do
end subroutine tokenize

subroutine add(tokens, lx, src, kind, first, last, sub)
    !! Append a token, maintaining the line count and the glue state for split tokens.
    type(token_list_t), intent(inout) :: tokens   !! token list to append to
    type(lexer_t), intent(inout) :: lx            !! lexer state
    character(len=*), intent(in) :: src           !! source text
    integer, intent(in) :: kind                   !! token kind, one of the TK_* constants
    integer, intent(in) :: first                  !! first byte of the token in `src`
    integer, intent(in) :: last                   !! last byte of the token in `src`
    integer, intent(in), optional :: sub          !! sub-kind (CM_*, CT_*, STR_*); 0 if absent
    type(token_t) :: t
    integer :: k

    t%kind = kind
    t%first = first
    t%last = last
    t%line = lx%line
    if (present(sub)) t%sub = sub

    if (kind == TK_CONT) then
        if (t%sub == CT_TRAILING) then
            lx%glue_pending = first > 1 .and. .not. is_blank(peek(src, first - 1))
        else
            ! a leading `&` directly followed by code joins it to the end of the previous line
            lx%glue_next = lx%glue_pending .and. is_code_char(peek(src, last + 1))
            lx%glue_pending = .false.
        end if
    else if (is_code_kind(kind)) then
        t%glued = lx%glue_next
        lx%glue_next = .false.
        lx%glue_pending = .false.
    end if

    do k = first, last
        if (src(k:k) == LF) then
            lx%line = lx%line + 1
        else if (src(k:k) == CR .and. peek(src, k + 1) /= LF) then
            lx%line = lx%line + 1
        end if
    end do
    call tokens%push(t)
end subroutine add

integer function string_end(src, i, flags) result(j)
    !! Last byte of a string literal whose opening quote is at `i`. Handles doubled quotes and
    !! continuation lines; `flags` reports STR_* conditions.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte of the opening quote
    integer, intent(out) :: flags         !! STR_* bit flags of the literal
    character :: q, c
    integer :: k

    flags = 0
    q = src(i:i)
    j = i + 1
    do
        if (j > len(src)) then
            flags = ior(flags, STR_UNTERMINATED)
            j = len(src)
            return
        end if
        c = src(j:j)
        if (c == q) then
            if (peek(src, j + 1) /= q) return
            j = j + 2
        else if (c == LF .or. c == CR) then
            flags = ior(flags, STR_UNTERMINATED)
            j = j - 1
            return
        else if (c == '&' .and. only_blanks_to_eol(src, j + 1)) then
            ! character context continued on the next line
            k = line_end(src, j) + 1          ! the line break
            if (k > len(src)) then
                j = len(src)
                flags = ior(flags, STR_UNTERMINATED)
                return
            end if
            if (src(k:k) == CR .and. peek(src, k + 1) == LF) k = k + 1
            k = k + 1                          ! start of the continuation line
            flags = ior(flags, STR_MULTILINE)
            j = k
            do while (is_blank(peek(src, j)))
                j = j + 1
            end do
            if (peek(src, j) == '&') then
                j = j + 1
            else
                flags = ior(flags, STR_RAW_CONT)
                j = k
            end if
        else
            j = j + 1
        end if
    end do
end function string_end

subroutine lex_number(src, i, j, kind)
    !! Integer or real literal starting at `i` (a digit, or `.` followed by a digit).
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first byte of the literal
    integer, intent(out) :: j             !! last byte of the literal, including any kind suffix
    integer, intent(out) :: kind          !! `TK_INT` or `TK_REAL`
    integer :: k
    character :: c

    kind = TK_INT
    j = digits_end(src, i)
    if (peek(src, j + 1) == '.' .and. .not. dotop_at(src, j + 1)) then
        kind = TK_REAL
        j = digits_end(src, j + 2)
    end if
    c = lower_char(peek(src, j + 1))
    if (c == 'e' .or. c == 'd' .or. c == 'q') then
        k = j + 2
        if (peek(src, k) == '+' .or. peek(src, k) == '-') k = k + 1
        if (is_digit(peek(src, k))) then
            kind = TK_REAL
            j = digits_end(src, k)
        end if
    end if
    if (peek(src, j + 1) == '_' .and. is_name_char(peek(src, j + 2))) j = name_end(src, j + 2)
end subroutine lex_number

integer function digits_end(src, i) result(j)
    !! Last digit of the run starting at `i`, or `i - 1` if there is none.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte where the run of digits starts
    j = i - 1
    do while (is_digit(peek(src, j + 1)))
        j = j + 1
    end do
end function digits_end

integer function name_end(src, i) result(j)
    !! Last byte of the name starting at `i`, stopping before inline fypp `${`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first byte of the name
    j = i
    do while (is_name_char(peek(src, j + 1)))
        if (peek(src, j + 1) == '$' .and. peek(src, j + 2) == '{') exit   ! inline fypp: name_${x}$
        j = j + 1
    end do
end function name_end

logical function dotop_at(src, i)
    !! True if a dot operator such as `.and.` or `.true.` starts at `i`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to look at
    integer :: j
    dotop_at = .false.
    if (peek(src, i) /= '.') return
    j = i + 1
    do while (is_letter(peek(src, j)))
        j = j + 1
    end do
    dotop_at = j > i + 1 .and. peek(src, j) == '.'
end function dotop_at

integer function dotop_end(src, i) result(j)
    !! Last byte of the dot operator at `i`, including a kind suffix on `.true._lk` / `.false._lk`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte of the opening `.`
    character(len=:), allocatable :: word
    j = i + 1
    do while (peek(src, j) /= '.')
        j = j + 1
    end do
    word = lowercase(src(i:j))
    if ((word == '.true.' .or. word == '.false.') .and. peek(src, j + 1) == '_' .and. &
        is_name_char(peek(src, j + 2))) j = name_end(src, j + 2)
end function dotop_end

integer function operator_end(src, i) result(j)
    !! Last byte of a two- or one-character operator at `i`, or 0 if there is none.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to look at
    character(len=*), parameter :: two(*) = ['**', '//', '==', '/=', '<=', '>=', '=>', '::']
    character(len=*), parameter :: one = '=+-*/<>()[],:;%?'
    character(len=2) :: pair
    integer :: k

    pair = src(i:min(i + 1, len(src)))
    do k = 1, size(two)
        if (pair == two(k)) then
            j = i + 1
            return
        end if
    end do
    j = 0
    if (index(one, src(i:i)) > 0) j = i
end function operator_end

integer function fypp_inline_end(src, i) result(j)
    !! Last byte of inline fypp (`${...}$`, `@{...}@`, `#{...}#`) starting at `i`, or 0.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to look at
    character :: c
    integer :: k

    j = 0
    c = src(i:i)
    if (index('$@#', c) == 0 .or. peek(src, i + 1) /= '{') return
    k = index(src(i + 2:), '}'//c)
    if (k > 0) j = i + 2 + k
end function fypp_inline_end

logical function is_preproc_start(src, i)
    !! True if a preprocessor line starts at `i` (the first non-blank of the line): cpp `#...`,
    !! fypp `#:`, `#!`, `$:` and `@:`. Inline fypp `#{` is not a directive line.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first non-blank byte of the line
    character :: c
    c = peek(src, i + 1)
    select case (src(i:i))
    case ('#')
        is_preproc_start = c /= '{' .and. c /= '}'
    case ('$', '@')
        is_preproc_start = c == ':'
    case default
        is_preproc_start = .false.
    end select
end function is_preproc_start

integer function preproc_end(src, i) result(j)
    !! Last byte of a preprocessor line at `i`, including continuation lines: a trailing `\` for
    !! cpp, a trailing `&` for fypp. The final line break is not included.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first byte of the directive (`#`, `$` or `@`)
    logical :: fypp
    integer :: k

    fypp = src(i:i) /= '#' .or. peek(src, i + 1) == ':' .or. peek(src, i + 1) == '!'
    j = line_end(src, i)
    do
        k = last_nonblank(src, i, j)
        if (k == 0) exit
        if (fypp) then
            if (src(k:k) /= '&') exit
        else
            if (src(k:k) /= '\') exit
        end if
        k = j + 1                              ! the line break
        if (k > len(src)) exit
        if (src(k:k) == CR .and. peek(src, k + 1) == LF) k = k + 1
        if (k + 1 > len(src)) exit
        j = line_end(src, k + 1)
        if (j < k + 1) j = k                   ! an empty continuation line
    end do
end function preproc_end

integer function comment_kind(text) result(kind)
    !! Comment sub-kind (one of the CM_* constants) of a comment.
    character(len=*), intent(in) :: text   !! comment text, from `!` to the end of the line
    character(len=:), allocatable :: low
    character(len=*), parameter :: prefixes(*) = [character(len=6) :: &
                                                  '!dir$', '!dec$', '!gcc$', '!ibm*', '!cdir$', '!pgi$', '!ms$', '!nec$']
    integer :: k

    kind = CM_PLAIN
    low = lowercase(text)
    if (len(low) >= 2) then
        select case (low(2:2))
        case ('$')
            if (is_letter(peek(low, 3))) then
                kind = CM_DIRECTIVE
            else
                kind = CM_CONDITIONAL
            end if
            return
        case ('&')
            kind = CM_FORMATTER
            return
        end select
    end if
    do k = 1, size(prefixes)
        if (index(low, trim(prefixes(k))) == 1) then
            kind = CM_DIRECTIVE
            return
        end if
    end do
end function comment_kind

pure integer function line_end(src, i) result(j)
    !! Index of the last byte before the line break at or after `i`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to start from
    j = i
    do while (j <= len(src))
        if (src(j:j) == LF .or. src(j:j) == CR) exit
        j = j + 1
    end do
    j = j - 1
end function line_end

pure integer function last_nonblank(src, i, j) result(k)
    !! Last non-blank byte in `src(i:j)`, or 0.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i, j           !! byte range to search
    do k = j, i, -1
        if (.not. is_blank(src(k:k))) return
    end do
    k = 0
end function last_nonblank

pure logical function only_blanks_to_eol(src, i)
    !! True if there are only blanks from `i` to the end of the line.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to start from
    integer :: j
    j = line_end(src, i)
    only_blanks_to_eol = last_nonblank(src, i, j) == 0
end function only_blanks_to_eol

pure character function peek(src, i)
    !! Character at `i`, or NUL outside the string.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte index, possibly outside `src`
    if (i < 1 .or. i > len(src)) then
        peek = achar(0)
    else
        peek = src(i:i)
    end if
end function peek

elemental logical function is_blank(c)
    !! True for a blank or tab.
    character, intent(in) :: c   !! the character
    is_blank = c == ' ' .or. c == TAB
end function is_blank

elemental logical function is_digit(c)
    !! True for `0` to `9`.
    character, intent(in) :: c   !! the character
    is_digit = c >= '0' .and. c <= '9'
end function is_digit

elemental logical function is_letter(c)
    !! True for an ASCII letter.
    character, intent(in) :: c   !! the character
    is_letter = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
end function is_letter

elemental logical function is_name_char(c)
    !! Characters allowed after the first letter of a name (`$` is a common extension).
    character, intent(in) :: c   !! the character
    is_name_char = is_letter(c) .or. is_digit(c) .or. c == '_' .or. c == '$'
end function is_name_char

elemental logical function is_quote(c)
    !! True for `'` or `"`.
    character, intent(in) :: c   !! the character
    is_quote = c == "'" .or. c == '"'
end function is_quote

elemental logical function is_code_char(c)
    !! A character that can be part of a code token (not a blank, line break or comment).
    character, intent(in) :: c   !! the character
    is_code_char = .not. (is_blank(c) .or. c == LF .or. c == CR .or. c == '!' .or. c == achar(0))
end function is_code_char

end module ffmt_lexer
