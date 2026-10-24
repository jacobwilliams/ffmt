module ffmt_linebreak
!! Splitting code lines that are longer than the line length.
!!
!! A line is split at a token boundary, preferring (in this order) after a comma, after an operator,
!! `=` or `::`, after an opening bracket, and anywhere between two tokens. Within the best kind of
!! break that fits, the rightmost is used. The first part ends with ` &` and the rest goes on a new
!! line; the layout pass then aligns it. Only the code counts: a trailing comment may extend past
!! the limit and moves with the last part. Lines are not split inside tokens (strings in particular),
!! in protected code, in comment or preprocessor lines, or when a token spans lines.
use ffmt_tokens
use ffmt_statements
use ffmt_source, only: source_t, parse_source
use ffmt_strings, only: string_builder_t, utf8_width
implicit none
private

public :: split_long_lines, long_code_lines

contains

function split_long_lines(src, line_length, indent, nsplit) result(out)
    !! `src` with each code line longer than `line_length` split once. The new continuation lines are indented
    !! by `indent` more than the line they come from if it is the first line of a statement, and like
    !! it if it is a continuation line, which the layout pass will usually leave in place. `nsplit`
    !! returns the number of splits.
    character(len=*), intent(in) :: src           !! the source text
    integer, intent(in) :: line_length            !! maximum line width, in characters
    integer, intent(in) :: indent                 !! indentation step, in blanks
    integer, intent(out) :: nsplit                !! number of lines split
    character(len=:), allocatable :: out

    type(source_t) :: source
    type(string_builder_t) :: buf
    character(len=:), allocatable :: nl
    integer :: k, j, lend, a, p, q, base, start_col, offset

    nsplit = 0
    call parse_source(src, source)
    nl = achar(10)
    do j = 1, source%tokens%n
        if (source%tokens%tok(j)%kind == TK_NEWLINE) then
            nl = token_text(src, source%tokens%tok(j))
            exit
        end if
    end do

    do k = 1, source%stmts%n
        if (source%keep(k) .or. source%stmts%s(k)%kind /= SK_CODE) then
            call copy(source%stmts%s(k)%first, source%stmts%s(k)%last)
            cycle
        end if
        j = source%stmts%s(k)%first
        do while (j <= source%stmts%s(k)%last)
            lend = j
            do while (lend < source%stmts%s(k)%last)
                if (source%tokens%tok(lend)%kind == TK_NEWLINE) exit
                lend = lend + 1
            end do
            ! split the line once; the rest is aligned by the next layout pass and split again if needed
            a = j
            start_col = column(j)
            base = indentation(j)
            offset = merge(indent, 0, j == source%stmts%s(k)%first)
            p = break_point(source%tokens, src, a, lend, start_col, line_length)
            if (p > 0) then
                call copy(a, p)
                call buf%append(' &'//nl//repeat(' ', base + offset))
                nsplit = nsplit + 1
                q = p + 1
                do while (source%tokens%tok(q)%kind == TK_SPACE)
                    q = q + 1
                end do
                a = q
            end if
            call copy(a, lend)
            j = lend + 1
        end do
    end do
    out = buf%get()

contains

    subroutine copy(first, last)
        !! Append the text of tokens `first:last` to the output.
        integer, intent(in) :: first, last   !! token range
        integer :: t
        do t = first, last
            call buf%append(token_text(src, source%tokens%tok(t)))
        end do
    end subroutine copy

    integer function column(t)
        !! Column of token `t` in `src`.
        integer, intent(in) :: t   !! index into the token list
        integer :: i
        i = scan(src(1:source%tokens%tok(t)%first - 1), achar(10)//achar(13), back=.true.)
        column = utf8_width(src(i + 1:source%tokens%tok(t)%first - 1))
    end function column

    integer function indentation(t)
        !! Indentation of the line containing token `t`.
        integer, intent(in) :: t   !! index into the token list
        integer :: i, m
        i = scan(src(1:source%tokens%tok(t)%first - 1), achar(10)//achar(13), back=.true.) + 1
        m = verify(src(i:), ' '//achar(9))
        if (m == 0) then
            indentation = 0
        else
            indentation = m - 1
        end if
    end function indentation

end function split_long_lines

integer function break_point(tokens, src, a, lend, start_col, line_length) result(best)
    !! The token after which to split tokens `a:lend` of a line that starts at token `a` in column
    !! `start_col`, or 0 if the code fits or cannot be split.
    type(token_list_t), intent(in) :: tokens      !! the tokens of `src`
    character(len=*), intent(in) :: src           !! the source text
    integer, intent(in) :: a                      !! first token of the line
    integer, intent(in) :: lend                   !! last token of the line, its line break if it has one
    integer, intent(in) :: start_col              !! column of token `a`
    integer, intent(in) :: line_length            !! maximum line width, in characters
    integer :: t, cend, p, q, prio, best_prio, end_col

    best = 0
    cend = 0
    do t = a, lend
        if (scan(token_text(src, tokens%tok(t)), achar(10)//achar(13)) > 0 .and. tokens%tok(t)%kind /= TK_NEWLINE) &
            return                                      ! a token continued on the next line
        if (tokens%tok(t)%kind == TK_COMMENT) exit
        if (is_code_kind(tokens%tok(t)%kind) .or. tokens%tok(t)%kind == TK_CONT) cend = t
    end do
    if (cend == 0) return
    if (end_column(cend) <= line_length) return

    best_prio = huge(1)
    do p = a, cend - 1
        if (.not. is_code_kind(tokens%tok(p)%kind)) cycle
        if (p == a .and. tokens%tok(p)%kind == TK_INT) cycle    ! a statement label
        q = p + 1
        do while (q <= cend)
            if (tokens%tok(q)%kind /= TK_SPACE) exit
            q = q + 1
        end do
        if (q > cend) cycle
        if (.not. is_code_kind(tokens%tok(q)%kind) .or. tokens%tok(q)%glued) cycle
        end_col = end_column(p)
        if (end_col + 2 > line_length) exit                     ! ` &` would not fit
        prio = priority(token_lower(src, tokens%tok(p)), tokens%tok(p)%kind)
        if (prio <= best_prio) then
            best_prio = prio
            best = p
        end if
    end do

contains

    integer function end_column(t)
        !! Column just after token `t`, for a line starting at token `a` in column `start_col`.
        integer, intent(in) :: t   !! index into the token list
        end_column = start_col + utf8_width(src(tokens%tok(a)%first:tokens%tok(t)%last))
    end function end_column

end function break_point

pure integer function priority(text, kind) result(prio)
    !! How good a place the end of a token with this text is for a line break (1 is best).
    character(len=*), intent(in) :: text   !! lowercase text of the token
    integer, intent(in) :: kind            !! token kind (`TK_*`)
    prio = 4
    select case (kind)
    case (TK_OP)
        select case (text)
        case (',')
            prio = 1
        case ('(', '[')
            prio = 3
        case (')', ']', '%', ':')
            prio = 4
        case default
            prio = 2
        end select
    case (TK_DOTOP)
        if (text /= '.true.' .and. text /= '.false.' .and. text /= '.not.') prio = 2
    end select
end function priority

function long_code_lines(src, line_length) result(lines)
    !! Line numbers of code lines whose code (without a trailing comment) is longer than `line_length`,
    !! outside protected code.
    character(len=*), intent(in) :: src           !! the source text
    integer, intent(in) :: line_length            !! maximum line width, in characters; 0 or less for no limit
    integer, allocatable :: lines(:)
    type(source_t) :: source
    integer :: k, t, col, last_line, line, p, q

    allocate (lines(0))
    if (line_length <= 0) return
    call parse_source(src, source)
    last_line = 0
    do k = 1, source%stmts%n
        if (source%keep(k) .or. source%stmts%s(k)%kind /= SK_CODE) cycle
        col = 0
        do t = source%stmts%s(k)%first, source%stmts%s(k)%last
            associate (tk => source%tokens%tok(t))
                if (tk%kind == TK_NEWLINE) then
                    col = 0
                    cycle
                end if
                if (t == source%stmts%s(k)%first) col = start_column(t)
                if (tk%kind == TK_COMMENT .or. tk%kind == TK_PREPROC) cycle
                ! a token continued on later lines (a string) is measured line by line
                line = tk%line
                p = tk%first
                do
                    q = index(src(p:tk%last), achar(10))
                    if (q == 0) exit
                    q = p + q - 1                     ! the line feed
                    if (q > p) then
                        if (src(q - 1:q - 1) == achar(13)) q = q - 1
                    end if
                    call check_width(tk, col + utf8_width(src(p:q - 1)), line)
                    col = 0
                    line = line + 1
                    p = p + index(src(p:tk%last), achar(10))
                end do
                col = col + utf8_width(src(p:tk%last))
                call check_width(tk, col, line)
            end associate
        end do
    end do

contains

    subroutine check_width(tk, width, line)
        !! Record `line` if it holds code token `tk` and its code is `width` characters wide, too long.
        type(token_t), intent(in) :: tk   !! the token
        integer, intent(in) :: width      !! width of the line up to the end of `tk` on it
        integer, intent(in) :: line       !! line number
        if (is_code_kind(tk%kind) .and. width > line_length .and. line /= last_line) then
            lines = [lines, line]
            last_line = line
        end if
    end subroutine check_width

    integer function start_column(t)
        !! Column of token `t`: the width of the text before it on its line.
        integer, intent(in) :: t   !! index into the token list
        integer :: i
        i = scan(src(1:source%tokens%tok(t)%first - 1), achar(10)//achar(13), back=.true.)
        start_column = utf8_width(src(i + 1:source%tokens%tok(t)%first - 1))
    end function start_column

end function long_code_lines

end module ffmt_linebreak
