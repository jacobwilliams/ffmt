module ffmt_diff
!! Unified diffs of two texts, for `--diff`.
!!
!! Lines are matched with a longest common subsequence computed by Hirschberg's algorithm, which
!! needs memory linear in the number of lines, so a diff of a file whose every line changed is
!! still cheap. Lines are compared as integers after hashing.
use, intrinsic :: iso_fortran_env, only: int64
use ffmt_strings, only: string_t, string_builder_t, to_s
implicit none
private

public :: unified_diff

integer, parameter :: OP_EQUAL = 0, OP_DELETE = 1, OP_INSERT = 2

type :: edit_t
    integer, allocatable :: op(:)   !! OP_EQUAL, OP_DELETE (line of a) or OP_INSERT (line of b)
    integer :: n = 0
end type edit_t

contains

function unified_diff(a, b, name, context) result(diff)
    !! The changes from `a` to `b` as a unified diff with `context` lines of context (default 3),
    !! with `name` in the headers. Empty if the texts are equal.
    character(len=*), intent(in) :: a, b, name
    integer, intent(in), optional :: context
    character(len=:), allocatable :: diff

    type(string_t), allocatable :: la(:), lb(:)
    integer, allocatable :: ia(:), ib(:)
    type(edit_t) :: e
    type(string_builder_t) :: buf
    integer :: ctx, k, first, last, i, j, ka, kb, na, nb, start_a, start_b

    diff = ''
    if (a == b .and. len(a) == len(b)) return
    ctx = 3
    if (present(context)) ctx = context
    la = split_lines(a)
    lb = split_lines(b)
    call line_ids(la, lb, ia, ib)
    allocate (e%op(size(ia) + size(ib)))
    call lcs(ia, 1, size(ia), ib, 1, size(ib), e)

    call buf%append('--- '//name//new_line('a')//'+++ '//name//new_line('a'))
    ! hunks: runs of edits with at most 2*ctx equal lines between them, plus context around
    k = 1
    i = 1           ! next line of a
    j = 1           ! next line of b
    do while (k <= e%n)
        if (e%op(k) == OP_EQUAL) then
            k = k + 1
            i = i + 1
            j = j + 1
            cycle
        end if
        first = k
        last = k
        do while (last < e%n)          ! extend over changes separated by short runs of equal lines
            ka = last + 1
            do while (ka <= e%n)
                if (e%op(ka) /= OP_EQUAL) exit
                ka = ka + 1
            end do
            if (ka > e%n .or. ka - last - 1 > 2*ctx) exit
            last = ka
        end do
        ! the hunk covers edits first-ctx .. last+ctx (equal lines at both ends)
        ka = first
        nb = 0
        do while (ka > 1 .and. nb < ctx)
            if (e%op(ka - 1) /= OP_EQUAL) exit
            ka = ka - 1
            nb = nb + 1
        end do
        start_a = i - nb
        start_b = j - nb
        kb = last
        nb = 0
        do while (kb < e%n .and. nb < ctx)
            if (e%op(kb + 1) /= OP_EQUAL) exit
            kb = kb + 1
            nb = nb + 1
        end do
        na = count(e%op(ka:kb) /= OP_INSERT)
        nb = count(e%op(ka:kb) /= OP_DELETE)
        call buf%append('@@ -'//range_text(start_a, na)//' +'//range_text(start_b, nb)//' @@'//new_line('a'))
        i = start_a
        j = start_b
        do k = ka, kb
            select case (e%op(k))
            case (OP_EQUAL)
                call add_line(' ', la(i)%s)
                i = i + 1
                j = j + 1
            case (OP_DELETE)
                call add_line('-', la(i)%s)
                i = i + 1
            case (OP_INSERT)
                call add_line('+', lb(j)%s)
                j = j + 1
            end select
        end do
        k = kb + 1
    end do
    diff = buf%get()

contains

    subroutine add_line(mark, line)
        character, intent(in) :: mark
        character(len=*), intent(in) :: line
        call buf%append(mark//line)
        if (len(line) == 0) then
            call buf%append(new_line('a')//'\ No newline at end of file'//new_line('a'))
        else if (line(len(line):len(line)) /= achar(10)) then
            call buf%append(new_line('a')//'\ No newline at end of file'//new_line('a'))
        end if
    end subroutine add_line

end function unified_diff

pure function range_text(start, n) result(text)
    !! `start,n` for a hunk header; `start` is the line before the hunk when it is empty.
    integer, intent(in) :: start, n
    character(len=:), allocatable :: text
    if (n == 0) then
        text = to_s(start - 1)//',0'
    else if (n == 1) then
        text = to_s(start)
    else
        text = to_s(start)//','//to_s(n)
    end if
end function range_text

function split_lines(text) result(lines)
    !! The lines of `text`, each with its line ending (the last one may have none).
    character(len=*), intent(in) :: text
    type(string_t), allocatable :: lines(:)
    integer :: i, j, n

    n = count([(text(i:i) == achar(10), i=1, len(text))])
    if (len(text) > 0) then
        if (text(len(text):len(text)) /= achar(10)) n = n + 1
    end if
    allocate (lines(n))
    i = 1
    do j = 1, n
        n = index(text(i:), achar(10))
        if (n == 0) then
            lines(j)%s = text(i:)
        else
            lines(j)%s = text(i:i + n - 1)
            i = i + n
        end if
    end do
end function split_lines

subroutine line_ids(la, lb, ia, ib)
    !! Number the distinct lines, so that equal lines have equal numbers.
    type(string_t), intent(in) :: la(:), lb(:)
    integer, allocatable, intent(out) :: ia(:), ib(:)
    integer, allocatable :: slot(:)     ! hash table: id of the line in each slot, or 0
    type(string_t), allocatable :: seen(:)
    integer :: k, nseen, mask

    mask = 1
    do while (mask < 2*(size(la) + size(lb)) + 1)
        mask = 2*mask
    end do
    allocate (slot(0:mask - 1), seen(size(la) + size(lb)), ia(size(la)), ib(size(lb)))
    mask = mask - 1
    slot = 0
    nseen = 0
    do k = 1, size(la)
        ia(k) = id_of(la(k)%s)
    end do
    do k = 1, size(lb)
        ib(k) = id_of(lb(k)%s)
    end do

contains

    integer function id_of(line)
        character(len=*), intent(in) :: line
        integer :: h
        h = int(iand(hash(line), int(mask, int64)))
        do
            if (slot(h) == 0) then
                nseen = nseen + 1
                seen(nseen)%s = line
                slot(h) = nseen
                id_of = nseen
                return
            end if
            if (len(seen(slot(h))%s) == len(line)) then
                if (seen(slot(h))%s == line) then
                    id_of = slot(h)
                    return
                end if
            end if
            h = iand(h + 1, mask)
        end do
    end function id_of

end subroutine line_ids

pure integer(int64) function hash(s) result(h)
    !! FNV-1a hash.
    character(len=*), intent(in) :: s
    integer :: k
    h = 1469598103_int64
    do k = 1, len(s)
        h = ieor(h, int(iachar(s(k:k)), int64))
        h = h*16777619_int64
        h = iand(h, 4294967295_int64)
    end do
end function hash

recursive subroutine lcs(a, a0, a1, b, b0, b1, e)
    !! Append the edits that turn `a(a0:a1)` into `b(b0:b1)` along a longest common subsequence
    !! (Hirschberg's algorithm).
    integer, intent(in) :: a(:), b(:)
    integer, intent(in) :: a0, a1, b0, b1
    type(edit_t), intent(inout) :: e
    integer :: lo_a, lo_b, hi_a, hi_b, nsuffix, k

    ! common prefix and suffix, without recursion
    lo_a = a0
    lo_b = b0
    do while (lo_a <= a1 .and. lo_b <= b1)
        if (a(lo_a) /= b(lo_b)) exit
        call push(OP_EQUAL)
        lo_a = lo_a + 1
        lo_b = lo_b + 1
    end do
    hi_a = a1
    hi_b = b1
    nsuffix = 0
    do while (hi_a >= lo_a .and. hi_b >= lo_b)
        if (a(hi_a) /= b(hi_b)) exit
        hi_a = hi_a - 1
        hi_b = hi_b - 1
        nsuffix = nsuffix + 1
    end do
    call middle(lo_a, hi_a, lo_b, hi_b)
    do k = 1, nsuffix
        call push(OP_EQUAL)
    end do

contains

    recursive subroutine middle(a0, a1, b0, b1)
        !! The edits for `a(a0:a1)` and `b(b0:b1)`, which differ at both ends.
        integer, intent(in) :: a0, a1, b0, b1
        integer :: fwd(0:b1 - b0 + 1), bwd(0:b1 - b0 + 1)   ! (not allocatable: assignment would make them 1-based)
        integer :: mid, k, best, split, j
        if (a0 > a1) then
            do j = b0, b1
                call push(OP_INSERT)
            end do
            return
        end if
        if (b0 > b1) then
            do j = a0, a1
                call push(OP_DELETE)
            end do
            return
        end if
        if (a0 == a1) then
            ! one line of a: keep it if it occurs in b
            do j = b0, b1
                if (b(j) == a(a0)) then
                    do k = b0, j - 1
                        call push(OP_INSERT)
                    end do
                    call push(OP_EQUAL)
                    do k = j + 1, b1
                        call push(OP_INSERT)
                    end do
                    return
                end if
            end do
            call push(OP_DELETE)
            do k = b0, b1
                call push(OP_INSERT)
            end do
            return
        end if

        mid = (a0 + a1)/2
        fwd = lcs_lengths(a(a0:mid), b(b0:b1))
        bwd = lcs_lengths(a(a1:mid + 1:-1), b(b1:b0:-1))
        best = -1
        split = 0
        do k = 0, b1 - b0 + 1
            if (fwd(k) + bwd(b1 - b0 + 1 - k) > best) then
                best = fwd(k) + bwd(b1 - b0 + 1 - k)
                split = k
            end if
        end do
        call lcs(a, a0, mid, b, b0, b0 + split - 1, e)
        call lcs(a, mid + 1, a1, b, b0 + split, b1, e)
    end subroutine middle

    subroutine push(op)
        integer, intent(in) :: op
        e%n = e%n + 1
        e%op(e%n) = op
    end subroutine push

end subroutine lcs

pure function lcs_lengths(a, b) result(row)
    !! `row(k)`: the length of the longest common subsequence of `a` and `b(1:k)`.
    integer, intent(in) :: a(:), b(:)
    integer :: row(0:size(b))
    integer :: prev(0:size(b))
    integer :: i, k
    row = 0
    do i = 1, size(a)
        prev = row
        do k = 1, size(b)
            if (a(i) == b(k)) then
                row(k) = prev(k - 1) + 1
            else
                row(k) = max(row(k - 1), prev(k))
            end if
        end do
    end do
end function lcs_lengths

end module ffmt_diff
