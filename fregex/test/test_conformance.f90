module test_conformance
!! Runs the conformance vectors in test/data, generated from Python's `re` by tools/gen_conformance.py.
use iso_fortran_env, only: error_unit
use testdrive, only: new_unittest, unittest_type, error_type, check
use fregex
implicit none
private

public :: collect_conformance

integer, parameter :: MAX_REPORTED = 20   !! failures printed in detail per file

contains

subroutine collect_conformance(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("features", test_features), &
        new_unittest("errors", test_errors), &
        new_unittest("fprettify patterns", test_fprettify), &
        new_unittest("random patterns", test_fuzz) &
        ]
end subroutine collect_conformance

subroutine test_features(error)
    type(error_type), allocatable, intent(out) :: error
    call run_file(error, 'test/data/features.txt')
end subroutine test_features

subroutine test_errors(error)
    type(error_type), allocatable, intent(out) :: error
    call run_file(error, 'test/data/errors.txt')
end subroutine test_errors

subroutine test_fprettify(error)
    type(error_type), allocatable, intent(out) :: error
    call run_file(error, 'test/data/fprettify.txt')
end subroutine test_fprettify

subroutine test_fuzz(error)
    type(error_type), allocatable, intent(out) :: error
    call run_file(error, 'test/data/fuzz.txt')
end subroutine test_fuzz

subroutine run_file(error, path)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: path

    integer :: unit, ios, ncases, nfail
    character(len=:), allocatable :: line, expected, actual
    logical :: exists

    inquire (file=path, exist=exists)
    if (.not. exists) then
        call check(error, .false., 'missing '//path//' (run tools/gen_conformance.py)')
        return
    end if
    open (newunit=unit, file=path, status='old', action='read', iostat=ios)
    ncases = 0
    nfail = 0
    do
        call read_line(unit, line, ios)
        if (ios /= 0) exit
        if (len(line) == 0) cycle
        if (line(1:1) == '#') cycle
        ncases = ncases + 1
        call run_case(line, expected, actual)
        if (actual /= expected .or. len(actual) /= len(expected)) then
            nfail = nfail + 1
            if (nfail <= MAX_REPORTED) call report_failure(line, expected, actual)
        end if
    end do
    close (unit)
    call check(error, nfail == 0, to_s(nfail)//' of '//to_s(ncases)//' cases in '//path//' failed')
end subroutine run_file

subroutine run_case(line, expected, actual)
    !! Run one case and return the expected and actual results in the vector file's format.
    character(len=*), intent(in) :: line
    character(len=:), allocatable, intent(out) :: expected, actual

    type(string_t), allocatable :: tok(:), parts(:)
    type(regex_t) :: re
    type(match_t) :: m
    type(match_t), allocatable :: ms(:)
    character(len=:), allocatable :: op, pattern, subject, repl, msg, out
    integer :: flags, stat, k, n
    logical :: found

    tok = split_words(line)
    op = tok(1)%s
    read (tok(2)%s, *) flags
    pattern = unhex(tok(3)%s)
    subject = unhex(tok(4)%s)
    repl = unhex(tok(5)%s)
    expected = tok(7)%s
    do k = 8, size(tok)
        expected = expected//' '//tok(k)%s
    end do

    call re%compile(pattern, flags, stat=stat, errmsg=msg)
    if (op == 'compile') then
        actual = merge('error   ', 'compiled', stat /= RE_OK)
        actual = trim(actual)
        return
    end if
    if (stat /= RE_OK) then
        actual = 'compile error: '//msg
        return
    end if

    select case (op)
    case ('search', 'match', 'fullmatch')
        select case (op)
        case ('search')
            found = re%search(subject, m, stat=stat)
        case ('match')
            found = re%match(subject, m, stat=stat)
        case default
            found = re%fullmatch(subject, m, stat=stat)
        end select
        if (.not. found) then
            actual = 'none'
        else
            actual = to_s(m%start(0))//' '//to_s(m%end(0))
            do k = 1, m%group_count()
                actual = actual//' '//to_s(m%start(k))//' '//to_s(m%end(k))
            end do
        end if
    case ('findall')
        ms = re%find_all(subject, stat=stat)
        actual = to_s(size(ms))
        do k = 1, size(ms)
            actual = actual//' '//to_s(ms(k)%start())//' '//to_s(ms(k)%end())
        end do
    case ('split')
        parts = re%split(subject, stat=stat)
        actual = to_s(size(parts))
        do k = 1, size(parts)
            actual = actual//' '//hex(parts(k)%s)
        end do
    case ('sub')
        out = re%sub(subject, repl, nsubs=n, stat=stat)
        if (stat == RE_ERR_TEMPLATE) then
            actual = 'error'
            return
        end if
        actual = to_s(n)//' '//hex(out)
    case default
        actual = 'unknown op '//op
    end select
    if (stat /= RE_OK) actual = 'runtime error '//to_s(stat)
end subroutine run_case

subroutine report_failure(line, expected, actual)
    character(len=*), intent(in) :: line, expected, actual
    type(string_t), allocatable :: tok(:)
    tok = split_words(line)
    write (error_unit, '(a)') '  FAIL '//tok(1)%s//' flags='//tok(2)%s// &
        ' pattern="'//printable(unhex(tok(3)%s))//'" subject="'//printable(unhex(tok(4)%s))//'"'
    if (tok(5)%s /= '.') write (error_unit, '(a)') '       repl="'//printable(unhex(tok(5)%s))//'"'
    write (error_unit, '(a)') '       expected: '//expected
    write (error_unit, '(a)') '       actual:   '//actual
end subroutine report_failure

subroutine read_line(unit, line, ios)
    !! Read a whole line of any length.
    integer, intent(in) :: unit
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: ios
    character(len=4096) :: buf
    integer :: sz

    line = ''
    do
        read (unit, '(a)', advance='no', iostat=ios, size=sz) buf
        line = line//buf(1:sz)
        if (ios /= 0) exit
    end do
    if (is_iostat_eor(ios)) ios = 0
    ! tolerate CRLF line endings
    if (len(line) > 0) then
        if (line(len(line):len(line)) == achar(13)) line = line(1:len(line) - 1)
    end if
end subroutine read_line

pure function split_words(line) result(words)
    character(len=*), intent(in) :: line
    type(string_t), allocatable :: words(:)
    integer :: i, j
    allocate (words(0))
    i = 1
    do while (i <= len(line))
        j = index(line(i:), ' ')
        if (j == 0) then
            words = [words, string_t(line(i:))]
            exit
        end if
        if (j > 1) words = [words, string_t(line(i:i + j - 2))]
        i = i + j
    end do
end function split_words

pure function unhex(h) result(s)
    character(len=*), intent(in) :: h
    character(len=:), allocatable :: s
    integer :: k, code
    if (h == '.') then
        s = ''
        return
    end if
    allocate (character(len=len(h)/2) :: s)
    do k = 1, len(h)/2
        read (h(2*k - 1:2*k), '(z2)') code
        s(k:k) = achar(code)
    end do
end function unhex

pure function hex(s) result(h)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: h
    integer :: k
    if (len(s) == 0) then
        h = '.'
        return
    end if
    allocate (character(len=2*len(s)) :: h)
    do k = 1, len(s)
        write (h(2*k - 1:2*k), '(z2.2)') iachar(s(k:k))
    end do
    h = lower(h)
end function hex

pure function lower(s) result(l)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: l
    integer :: k
    l = s
    do k = 1, len(s)
        if (s(k:k) >= 'A' .and. s(k:k) <= 'Z') l(k:k) = achar(iachar(s(k:k)) + 32)
    end do
end function lower

pure function printable(s) result(p)
    !! Show control and non-ASCII bytes as \xhh.
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: p
    integer :: k, c
    character(len=2) :: hh
    p = ''
    do k = 1, len(s)
        c = iachar(s(k:k))
        if (c < 32 .or. c > 126) then
            write (hh, '(z2.2)') c
            p = p//'\x'//hh
        else
            p = p//s(k:k)
        end if
    end do
end function printable

pure function to_s(i) result(s)
    integer, intent(in) :: i
    character(len=:), allocatable :: s
    character(len=16) :: buf
    write (buf, '(i0)') i
    s = trim(buf)
end function to_s

end module test_conformance
