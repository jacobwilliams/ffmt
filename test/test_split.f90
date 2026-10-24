module test_split
!! Tests of splitting long lines and of the spelling of relational operators.
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_format
use ffmt_options
implicit none
private

public :: collect_split

character, parameter :: NL = achar(10)

contains

subroutine collect_split(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("split after commas", test_commas), &
        new_unittest("split long expressions", test_expressions), &
        new_unittest("lines that are not split", test_not_split), &
        new_unittest("split lines are stable", test_stable), &
        new_unittest("relational symbols", test_symbols), &
        new_unittest("relational letters", test_letters) &
        ]
end subroutine collect_split

function lines(text) result(s)
    character(len=*), intent(in) :: text(:)
    character(len=:), allocatable :: s
    integer :: k
    s = ''
    do k = 1, size(text)
        s = s//trim(text(k))//NL
    end do
end function lines

subroutine expect(error, input, expected, opts)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: input, expected
    type(format_options_t), intent(in) :: opts
    character(len=:), allocatable :: out, errmsg
    integer :: stat
    call format_text(input, opts, out, stat, errmsg)
    call check(error, stat, FMT_OK)
    if (allocated(error)) return
    call check(error, out == expected, 'got:'//NL//out//'expected:'//NL//expected)
end subroutine expect

subroutine test_commas(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%line_length = 40
    ! the rightmost comma that fits, then aligned with the first argument
    call expect(error, lines([character(len=80) :: &
                              'call compute(alpha, beta, gamma, delta, epsilon)']), &
                lines([character(len=80) :: &
                       'call compute(alpha, beta, gamma, &', &
                       '             delta, epsilon)']), opts)
    if (allocated(error)) return
    ! a remainder that is still too long is split again
    call expect(error, lines([character(len=120) :: &
                              'call compute(alpha, beta, gamma, delta, epsilon, zeta, eta, theta, iota, kappa)']), &
                lines([character(len=80) :: &
                       'call compute(alpha, beta, gamma, &', &
                       '             delta, epsilon, zeta, &', &
                       '             eta, theta, iota, kappa)']), opts)
end subroutine test_commas

subroutine test_expressions(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%line_length = 30
    ! after an operator when there is no comma; a trailing comment goes with the last part
    call expect(error, lines([character(len=80) :: &
                              'subroutine s', &
                              'total = first_value + second_value  ! sum', &
                              'end subroutine']), &
                lines([character(len=80) :: &
                       'subroutine s', &
                       '    total = first_value + &', &
                       '            second_value  ! sum', &
                       'end subroutine']), opts)
end subroutine test_expressions

subroutine test_not_split(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: out, errmsg
    type(diagnostic_t), allocatable :: diags(:)
    integer :: stat

    opts%line_length = 20
    ! code that fits, with a long comment; a long comment line; protected code
    call expect(error, lines([character(len=80) :: &
                              'x = 1  ! a comment that goes past the limit', &
                              '! a comment line that goes past the limit', &
                              'call f(aaaa, bbbb, cccc)  !&']), &
                lines([character(len=80) :: &
                       'x = 1  ! a comment that goes past the limit', &
                       '! a comment line that goes past the limit', &
                       'call f(aaaa, bbbb, cccc)  !&']), opts)
    if (allocated(error)) return
    ! a string longer than the limit cannot be split, and is reported
    call format_text("s = 'a long string that does not fit'"//NL, opts, out, stat, errmsg, diags)
    call check(error, out, 's = &'//NL//"    'a long string that does not fit'"//NL)
    if (allocated(error)) return
    call check(error, size(diags), 1)
    if (allocated(error)) return
    call check(error, diags(1)%line, 2)
    if (allocated(error)) return
    ! no splitting when disabled
    opts%split_lines = .false.
    call expect(error, lines([character(len=80) :: 'call f(aaaa, bbbb, cccc)']), &
                lines([character(len=80) :: 'call f(aaaa, bbbb, cccc)']), opts)
end subroutine test_not_split

subroutine test_stable(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: once, twice, errmsg, src
    integer :: stat
    opts%line_length = 30
    src = lines([character(len=120) :: &
                 'module m', 'contains', 'subroutine s(a, b, c)', &
                 'if (a > b .and. b > c .and. c > 0) call report(a, b, c, "all positive")', &
                 'x(1:n) = [(real(i, kind=8)*2.0 + offset, i = 1, n)]', &
                 'end subroutine', 'end module'])
    call format_text(src, opts, once, stat, errmsg)
    call format_text(once, opts, twice, stat, errmsg)
    call check(error, twice == once, 'second pass changed:'//NL//once//NL//twice)
end subroutine test_stable

subroutine test_symbols(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%relational = REL_SYMBOLS
    call expect(error, lines([character(len=80) :: &
                              'if (a .EQ. b .and. c .ne. d .or. e.lt.f) x = g .GE. h', &
                              'interface operator(.lt.)', &
                              'end interface', &
                              'call k<<<1, 2>>>(x)', &
                              'y = a.le.b  !&']), &
                lines([character(len=80) :: &
                       'if (a == b .and. c /= d .or. e<f) x = g >= h', &
                       'interface operator(<)', &
                       'end interface', &
                       'call k<<<1, 2>>>(x)', &
                       'y = a.le.b  !&']), opts)
end subroutine test_symbols

subroutine test_letters(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%relational = REL_LETTERS
    call expect(error, lines([character(len=80) :: 'if (a==b .or. c<=d .or. 1<2) x = 1']), &
                lines([character(len=80) :: 'if (a.eq.b .or. c.le.d .or. 1.lt.2) x = 1']), opts)
end subroutine test_letters

end module test_split
