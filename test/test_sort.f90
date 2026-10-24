module test_sort
!! Tests of sorting `use` statements and `only:` lists.
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_format
use ffmt_options
implicit none
private

public :: collect_sort

character, parameter :: NL = achar(10)

contains

subroutine collect_sort(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("sort use statements", test_statements), &
        new_unittest("use statements that stay", test_statements_kept), &
        new_unittest("sort only lists", test_only), &
        new_unittest("only lists that stay", test_only_kept), &
        new_unittest("sorted only lists are split", test_only_split), &
        new_unittest("safety check with reordering", test_same_code) &
        ]
end subroutine collect_sort

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
    !! Formatting `input` gives `expected`, and formatting that again changes nothing.
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: input, expected
    type(format_options_t), intent(in) :: opts
    character(len=:), allocatable :: out, again, errmsg
    integer :: stat
    call format_text(input, opts, out, stat, errmsg)
    call check(error, stat, FMT_OK)
    if (allocated(error)) return
    call check(error, out == expected, 'got:'//NL//out//'expected:'//NL//expected)
    if (allocated(error)) return
    call format_text(out, opts, again, stat, errmsg)
    call check(error, again == out, 'not stable:'//NL//again)
end subroutine expect

subroutine test_statements(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%sort_use = .true.
    ! by module name ignoring case, with continuation lines and comments; the same module keeps its order
    call expect(error, lines([character(len=80) :: &
                              'module m', &
                              'use zeta, only: z', &
                              'use, intrinsic :: iso_fortran_env', &
                              'use :: Beta, only: b2', &
                              'use alpha, only: a1, &', &
                              '                 a2  ! alpha', &
                              'use beta, only: b1', &
                              'implicit none', &
                              'end module m']), &
                lines([character(len=80) :: &
                       'module m', &
                       'use alpha, only: a1, &', &
                       '                 a2  ! alpha', &
                       'use :: Beta, only: b2', &
                       'use beta, only: b1', &
                       'use, intrinsic :: iso_fortran_env', &
                       'use zeta, only: z', &
                       'implicit none', &
                       'end module m']), opts)
end subroutine test_statements

subroutine test_statements_kept(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: src
    opts%sort_use = .true.
    ! blank lines, comments and preprocessor lines end a group; protected and `;` statements stay
    src = lines([character(len=80) :: &
                 'module m', &
                 'use d', &
                 'use c', &
                 '', &
                 'use b', &
                 '! a comment', &
                 'use a', &
                 '#ifdef X', &
                 'use z', &
                 '#endif', &
                 'use y', &
                 'use x  !&', &
                 'use w', &
                 'use v; use u', &
                 'use t', &
                 'implicit none', &
                 'end module m'])
    call expect(error, src, lines([character(len=80) :: &
                                   'module m', &
                                   'use c', &
                                   'use d', &
                                   '', &
                                   'use b', &
                                   '! a comment', &
                                   'use a', &
                                   '#ifdef X', &
                                   'use z', &
                                   '#endif', &
                                   'use y', &
                                   'use x  !&', &
                                   'use w', &
                                   'use v; use u', &
                                   'use t', &
                                   'implicit none', &
                                   'end module m']), opts)
    if (allocated(error)) return
    ! off by default
    call expect(error, src, src, format_options_t())
end subroutine test_statements_kept

subroutine test_only(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%sort_use_only = .true.
    ! by local name ignoring case; a list on several lines is joined; the comma style is kept
    call expect(error, lines([character(len=80) :: &
                              'use m, only: d => x, C, operator(+), &', &
                              '             b, a  ! names', &
                              'use n, only:z,y']), &
                lines([character(len=80) :: &
                       'use m, only: a, b, C, d => x, operator(+)  ! names', &
                       'use n, only:y,z']), opts)
end subroutine test_only

subroutine test_only_kept(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: src
    opts%sort_use_only = .true.
    ! sorted lists keep their layout; comments before the last line and protected statements stay
    src = lines([character(len=80) :: &
                 'use m, only: a, &', &
                 '             b', &
                 'use n, only: d, & ! first', &
                 '             c', &
                 'use o, only: f, e  !&'])
    call expect(error, src, src, opts)
end subroutine test_only_kept

subroutine test_only_split(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%sort_use_only = .true.
    opts%line_length = 40
    call expect(error, lines([character(len=80) :: &
                              'use m, only: zeta, epsilon, delta, &', &
                              '             gamma, beta, alpha']), &
                lines([character(len=80) :: &
                       'use m, only: alpha, beta, delta, &', &
                       '             epsilon, gamma, zeta']), opts)
    if (allocated(error)) return
    ! without line splitting, the sorted list is broken at commas to fit, aligned after `only:`
    opts%split_lines = .false.
    call expect(error, lines([character(len=80) :: &
                              'subroutine s', &
                              '    use m, only: zeta, epsilon, delta, gamma, beta, alpha', &
                              'end subroutine s']), &
                lines([character(len=80) :: &
                       'subroutine s', &
                       '    use m, only: alpha, beta, delta, &', &
                       '                 epsilon, gamma, zeta', &
                       'end subroutine s']), opts)
end subroutine test_only_split

subroutine test_same_code(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), parameter :: a = 'use b'//NL//'use a, only: y, x'//NL//'x = 1'//NL
    ! reordering is only accepted when asked for, and only within a run of use statements
    call check(error, .not. same_code(a, 'use a, only: x, y'//NL//'use b'//NL//'x = 1'//NL))
    if (allocated(error)) return
    call check(error, same_code(a, 'use a, only: x, y'//NL//'use b'//NL//'x = 1'//NL, .true.))
    if (allocated(error)) return
    call check(error, .not. same_code(a, 'use b'//NL//'x = 1'//NL//'use a, only: y, x'//NL, .true.))
    if (allocated(error)) return
    call check(error, .not. same_code(a, 'use b'//NL//'use a, only: y, z'//NL//'x = 1'//NL, .true.))
    if (allocated(error)) return
    call check(error, .not. same_code(a, 'use b'//NL//'use a, only: x'//NL//'x = 1'//NL, .true.))
end subroutine test_same_code

end module test_sort
