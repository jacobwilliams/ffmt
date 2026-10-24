module test_case
!! Tests of the letter case of keywords and operators.
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_format
use ffmt_options
implicit none
private

public :: collect_case

character, parameter :: NL = achar(10)

contains

subroutine collect_case(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("keywords lower", test_lower), &
        new_unittest("keywords upper", test_upper), &
        new_unittest("names are not keywords", test_names), &
        new_unittest("operators", test_operators), &
        new_unittest("keep by default", test_keep) &
        ]
end subroutine collect_case

subroutine expect(error, input, expected, kw, ops)
    !! Format `input` lines (joined with line feeds) with the given case modes.
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: input(:), expected(:)
    integer, intent(in) :: kw, ops
    type(format_options_t) :: o
    character(len=:), allocatable :: out, errmsg, src, want
    integer :: stat, k

    o%letter_case%keywords = kw
    o%letter_case%operators = ops
    src = ''
    want = ''
    do k = 1, size(input)
        src = src//trim(input(k))//NL
        want = want//trim(expected(k))//NL
    end do
    call format_text(src, o, out, stat, errmsg)
    call check(error, stat, FMT_OK)
    if (allocated(error)) return
    call check(error, out == want, 'got:'//NL//out//'expected:'//NL//want)
end subroutine expect

subroutine test_lower(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, [character(len=60) :: &
                        'MODULE M', &
                        'USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: WP => REAL64', &
                        'IMPLICIT NONE', &
                        'CONTAINS', &
                        'PURE REAL(KIND=WP) FUNCTION F(X) RESULT(Y)', &
                        'REAL(WP), INTENT(IN), DIMENSION(:) :: X', &
                        'CHARACTER(LEN=*), PARAMETER :: S = "ABC"', &
                        'IF (X(1) > 0) THEN', &
                        'Y = SUM(X)', &
                        'ELSE IF (X(1) < 0) THEN', &
                        'CALL FOO(X)', &
                        'ELSE', &
                        'GO TO 10', &
                        'ENDIF', &
                        'END FUNCTION F', &
                        'END MODULE M'], &
                [character(len=60) :: &
                 'module M', &
                 'use, intrinsic :: ISO_FORTRAN_ENV, only: WP => REAL64', &
                 'implicit none', &
                 'contains', &
                 'pure real(kind=WP) function F(X) result(Y)', &
                 '    real(WP), intent(in), dimension(:) :: X', &
                 '    character(len=*), parameter :: S = "ABC"', &
                 '    if (X(1) > 0) then', &
                 '        Y = SUM(X)', &
                 '    else if (X(1) < 0) then', &
                 '        call FOO(X)', &
                 '    else', &
                 '        go to 10', &
                 '    endif', &
                 'end function F', &
                 'end module M'], CASE_LOWER, CASE_KEEP)
end subroutine test_lower

subroutine test_upper(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, [character(len=40) :: &
                        'do i = 1, n', &
                        'select case (i)', &
                        'case default', &
                        'if (x) call f()', &
                        'end select', &
                        'enddo'], &
                [character(len=40) :: &
                 'DO i = 1, n', &
                 '    SELECT CASE (i)', &
                 '    CASE DEFAULT', &
                 '        IF (x) CALL f()', &
                 '    END SELECT', &
                 'ENDDO'], CASE_UPPER, CASE_KEEP)
end subroutine test_upper

subroutine test_names(error)
    type(error_type), allocatable, intent(out) :: error
    ! words that name things keep their case, even when spelled like keywords
    call expect(error, [character(len=40) :: &
                        'CALL OPEN(DO)', &
                        'REAL = INTEGER + 1', &
                        'IF (REAL > 0) STOP = 1', &
                        'WRITE(*,*) REAL(X)', &
                        'SUBROUTINE END', &
                        'LOGICAL SAVE, DATA', &
                        'END SUBROUTINE END'], &
                [character(len=40) :: &
                 'call OPEN(DO)', &
                 'REAL = INTEGER + 1', &
                 'if (REAL > 0) STOP = 1', &
                 'write(*,*) REAL(X)', &
                 'subroutine END', &
                 '    logical SAVE, DATA', &
                 'end subroutine END'], CASE_LOWER, CASE_KEEP)
end subroutine test_names

subroutine test_operators(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, [character(len=40) :: 'x = a .AND. .Not. B .eq. .TRUE._lk'], &
                [character(len=40) :: 'x = a .and. .not. B .eq. .true._lk'], CASE_KEEP, CASE_LOWER)
end subroutine test_operators

subroutine test_keep(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, [character(len=40) :: 'IF (X .EQ. 1) Y = 2'], &
                [character(len=40) :: 'IF (X .EQ. 1) Y = 2'], CASE_KEEP, CASE_KEEP)
end subroutine test_keep

end module test_case
