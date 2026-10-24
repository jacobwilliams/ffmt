module test_whitespace
!! Tests of the whitespace rules within lines.
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_format
use ffmt_options
implicit none
private

public :: collect_whitespace

character, parameter :: NL = achar(10)

contains

subroutine collect_whitespace(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("defaults", test_defaults), &
        new_unittest("defaults keep", test_defaults_keep), &
        new_unittest("operators", test_operators), &
        new_unittest("commas", test_commas), &
        new_unittest("not operators", test_not_operators), &
        new_unittest("joins", test_joins), &
        new_unittest("remove everything", test_remove_all), &
        new_unittest("conditional expressions", test_conditional), &
        new_unittest("user-defined operators", test_defined_operators) &
        ]
end subroutine collect_whitespace

subroutine expect(error, input, expected, opts)
    !! Format the single line `input` and compare with `expected`.
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: input, expected
    type(format_options_t), intent(in), optional :: opts
    type(format_options_t) :: o
    character(len=:), allocatable :: out, errmsg
    integer :: stat

    if (present(opts)) o = opts
    call format_text(input//NL, o, out, stat, errmsg)
    call check(error, stat, FMT_OK)
    if (allocated(error)) return
    call check(error, out == expected//NL, 'input:    '//input//NL//'  got:      '//out//'  expected: '//expected)
end subroutine expect

function all_rules(mode) result(opts)
    !! Options with every whitespace rule set to `mode`.
    integer, intent(in) :: mode
    type(format_options_t) :: opts
    opts%spaces = whitespace_options_t(mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, &
                                       mode, mode, mode, mode)
end function all_rules

subroutine test_defaults(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, 'x=1', 'x = 1')
    if (allocated(error)) return
    call expect(error, 'call f(a = 1, b= 2)', 'call f(a=1, b=2)')
    if (allocated(error)) return
    call expect(error, 'integer,intent(in)::n', 'integer,intent(in) :: n')
    if (allocated(error)) return
    call expect(error, 'p=>q', 'p => q')
    if (allocated(error)) return
    call expect(error, 'y = a ** 2 // b', 'y = a**2//b')
    if (allocated(error)) return
    call expect(error, 'y = a(1 : n, :) % x', 'y = a(1:n, :)%x')
    if (allocated(error)) return
    call expect(error, 'if(x) y = 1', 'if (x) y = 1')
    if (allocated(error)) return
    call expect(error, 'outer: do while(x)', 'outer: do while (x)')
    if (allocated(error)) return
    call expect(error, 'select case(i)', 'select case (i)')
    if (allocated(error)) return
    call expect(error, 'else if(x)then', 'else if (x) then')
    if (allocated(error)) return
    call expect(error, 'use m, only: a=>b', 'use m, only: a => b')
    if (allocated(error)) return
    call expect(error, 'do i=1, n', 'do i = 1, n')
end subroutine test_defaults

subroutine test_defaults_keep(error)
    type(error_type), allocatable, intent(out) :: error
    ! mixed conventions are left as they are, and extra blanks (alignment) are kept
    call expect(error, 'x = a+b - c*d / e', 'x = a+b - c*d / e')
    if (allocated(error)) return
    call expect(error, 'if (a==b .and. c /= d) y = 1', 'if (a==b .and. c /= d) y = 1')
    if (allocated(error)) return
    call expect(error, 'call f(a,b, c)', 'call f(a,b, c)')
    if (allocated(error)) return
    call expect(error, 'integer          :: n   =  1', 'integer          :: n   =  1')
    if (allocated(error)) return
    call expect(error, 'call g()  ! x=1', 'call g()  ! x=1')
end subroutine test_defaults_keep

subroutine test_operators(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%spaces%plus_minus = WS_ADD
    opts%spaces%mult_div = WS_ADD
    opts%spaces%relational = WS_ADD
    opts%spaces%logical = WS_ADD
    ! unary minus and exponents in literals are not binary operators
    call expect(error, 'x = -a+b*2-1.0e-3', 'x = -a + b * 2 - 1.0e-3', opts)
    if (allocated(error)) return
    call expect(error, 'y = f(-x)/(a-b)', 'y = f(-x) / (a - b)', opts)
    if (allocated(error)) return
    call expect(error, 'if (a<b.and.c>=d.or..not.e) z = a.eq.b', 'if (a < b .and. c >= d .or. .not.e) z = a .eq. b', opts)
    if (allocated(error)) return
    call expect(error, 'x = 2*-1', 'x = 2 * -1', opts)
end subroutine test_operators

subroutine test_commas(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%spaces%comma = WS_ADD
    call expect(error, 'call f(a,b ,c)', 'call f(a, b, c)', opts)
    if (allocated(error)) return
    call expect(error, 'call f(a, &', 'call f(a, &', opts)
    if (allocated(error)) return
    opts%spaces%comma = WS_REMOVE
    call expect(error, 'call f(a, b , c)', 'call f(a,b,c)', opts)
end subroutine test_commas

subroutine test_not_operators(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts = all_rules(WS_ADD)
    ! `*` in type selectors, `print *`, assumed sizes; `/` in data, common and array constructors
    call expect(error, 'real*8 :: x(*)', 'real*8 :: x(*)', opts)
    if (allocated(error)) return
    call expect(error, 'character*(*) :: s', 'character*(*) :: s', opts)
    if (allocated(error)) return
    call expect(error, 'print *, x', 'print *, x', opts)
    if (allocated(error)) return
    call expect(error, 'write(*,*) x', 'write(*, *) x', opts)
    if (allocated(error)) return
    call expect(error, 'data x/1.0/, y/2.0/', 'data x/1.0/, y/2.0/', opts)
    if (allocated(error)) return
    call expect(error, 'common /blk/ a,b', 'common /blk/ a, b', opts)
    if (allocated(error)) return
    call expect(error, 'x = (/1,2/)', 'x = (/1, 2/)', opts)
    if (allocated(error)) return
    ! format statements, strings and CUDA chevrons are left alone
    call expect(error, '10 format(1x,a,i5/)', '10 format(1x,a,i5/)', opts)
    if (allocated(error)) return
    call expect(error, "s = 'a=b,c'", "s = 'a=b,c'", opts)
    if (allocated(error)) return
    call expect(error, 'call k<<<n,m>>>(x)', 'call k<<<n, m>>>(x)', opts)
    if (allocated(error)) return
    ! `type(t)` and `case` default are not control statements
    call expect(error, 'type(t) :: x', 'type(t) :: x', opts)
    if (allocated(error)) return
    call expect(error, 'case(1)', 'case (1)', opts)
end subroutine test_not_operators

subroutine test_joins(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts = all_rules(WS_REMOVE)
    ! blanks are kept where removing them would merge tokens: `1 .eq.` is not `1.eq.`
    call expect(error, 'x = 1 .eq. y', 'x=1 .eq.y', opts)
    if (allocated(error)) return
    ! an operator without an operand on the same line is left alone
    call expect(error, 'x = a + &', 'x=a + &', opts)
end subroutine test_joins

subroutine test_remove_all(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts = all_rules(WS_REMOVE)
    call expect(error, 'if (a + b == c) call f(x, y = 1)', 'if(a+b==c) call f(x,y=1)', opts)
end subroutine test_remove_all

subroutine test_conditional(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    call expect(error, 'x = (a > 0?a:-b)', 'x = (a > 0 ? a : -b)')
    if (allocated(error)) return
    call expect(error, 'y = (c?1:d?2:3)', 'y = (c ? 1 : d ? 2 : 3)')
    if (allocated(error)) return
    ! a slice inside a conditional expression is still a slice
    call expect(error, 'z = (c ? a(1 : 2) : b)', 'z = (c ? a(1:2) : b)')
    if (allocated(error)) return
    call expect(error, 'w = a(i : j)', 'w = a(i:j)')
    if (allocated(error)) return
    opts%spaces%conditional = WS_REMOVE
    call expect(error, 'x = (c ? a : b)', 'x = (c?a:b)', opts)
end subroutine test_conditional

subroutine test_defined_operators(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    call expect(error, 'x = a.plus.b', 'x = a.plus.b')       ! kept by default
    if (allocated(error)) return
    opts%spaces%defined_operators = WS_ADD
    call expect(error, 'x = a.plus.b', 'x = a .plus. b', opts)
    if (allocated(error)) return
    ! unary operators, logical constants with a kind and `operator(...)` are left alone
    call expect(error, 'y = .inv.b', 'y = .inv.b', opts)
    if (allocated(error)) return
    call expect(error, 'f = a.and..true._lk', 'f = a.and..true._lk', opts)
    if (allocated(error)) return
    call expect(error, 'interface operator(.plus.)', 'interface operator(.plus.)', opts)
    if (allocated(error)) return
    opts%spaces%defined_operators = WS_REMOVE
    call expect(error, 'x = a .plus. b', 'x = a.plus.b', opts)
end subroutine test_defined_operators

end module test_whitespace
