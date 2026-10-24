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
        new_unittest("keep by default", test_keep), &
        new_unittest("intrinsic procedures", test_procedures), &
        new_unittest("intrinsic module constants", test_constants), &
        new_unittest("kinds", test_kinds) &
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

subroutine expect_with(error, input, expected, o)
    !! Format the single line `input` with options `o` and compare with `expected`.
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: input, expected
    type(format_options_t), intent(in) :: o
    character(len=:), allocatable :: out, errmsg
    integer :: stat
    call format_text(input//NL, o, out, stat, errmsg)
    call check(error, stat, FMT_OK)
    if (allocated(error)) return
    call check(error, out == expected//NL, 'input:    '//input//NL//'  got:      '//out//'  expected: '//expected)
end subroutine expect_with

subroutine test_procedures(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: o
    o%letter_case%procedures = CASE_UPPER
    call expect_with(error, 'n = size(a) + len_trim(s)', 'n = SIZE(a) + LEN_TRIM(s)', o)
    if (allocated(error)) return
    call expect_with(error, 'call cpu_time(t)', 'call CPU_TIME(t)', o)
    if (allocated(error)) return
    call expect_with(error, 'call c_f_pointer(p, x)', 'call C_F_POINTER(p, x)', o)
    if (allocated(error)) return
    ! not calls: an array named like an intrinsic, a component, a declared entity, a definition
    call expect_with(error, 'size(i) = 1', 'size(i) = 1', o)
    if (allocated(error)) return
    call expect_with(error, 'x = a%size(1)', 'x = a%size(1)', o)
    if (allocated(error)) return
    call expect_with(error, 'integer :: size(10), count = 0', 'integer :: size(10), count = 0', o)
    if (allocated(error)) return
    call expect_with(error, 'integer :: n = size(a)', 'integer :: n = SIZE(a)', o)
    if (allocated(error)) return
    call expect_with(error, 'function sqrt(x)', 'function sqrt(x)', o)
    if (allocated(error)) return
    o%letter_case%procedures = CASE_LOWER
    call expect_with(error, 'x = ABS(y)', 'x = abs(y)', o)
end subroutine test_procedures

subroutine test_constants(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: o
    o%letter_case%constants = CASE_UPPER
    call expect_with(error, 'write (output_unit, *) huge(1_c_int)', 'write (OUTPUT_UNIT, *) huge(1_c_int)', o)
    if (allocated(error)) return
    call expect_with(error, 'use iso_fortran_env, only: int64, wp => real64', &
                     'use iso_fortran_env, only: INT64, wp => REAL64', o)
    if (allocated(error)) return
    call expect_with(error, 'type(c_ptr) :: p = c_null_ptr', 'type(C_PTR) :: p = C_NULL_PTR', o)
    if (allocated(error)) return
    ! a local entity named like a constant, and a keyword argument
    call expect_with(error, 'integer :: int64 = 8', 'integer :: int64 = 8', o)
    if (allocated(error)) return
    call expect_with(error, 'call f(output_unit=3)', 'call f(output_unit=3)', o)
end subroutine test_constants

subroutine test_kinds(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: o
    o%letter_case%types = CASE_UPPER
    call expect_with(error, 'real(dp) :: x = 1.0e-3_dp', 'real(DP) :: x = 1.0E-3_DP', o)
    if (allocated(error)) return
    call expect_with(error, 'y = int(x, kind=int8) + 2_ik', 'y = int(x, kind=INT8) + 2_IK', o)
    if (allocated(error)) return
    call expect_with(error, "l = .true._lk; s = ck_'Abc'", "l = .true._LK; s = CK_'Abc'", o)
    if (allocated(error)) return
    ! a conversion is not a declaration, and BOZ literals have no kind
    call expect_with(error, "y = real(i) + z'ff'", "y = real(i) + z'ff'", o)
    if (allocated(error)) return
    ! with operators in lowercase, the kind of a logical constant still follows the kinds
    o%letter_case%operators = CASE_LOWER
    call expect_with(error, 'l = .TRUE._lk', 'l = .true._LK', o)
end subroutine test_kinds

end module test_case
