program tester
use iso_fortran_env, only: error_unit
use testdrive, only: run_testsuite, new_testsuite, testsuite_type
use test_lexer, only: collect_lexer
use test_structure, only: collect_structure
use test_layout, only: collect_layout
use test_whitespace, only: collect_whitespace
use test_case, only: collect_case
use test_split, only: collect_split
use test_tools, only: collect_tools
implicit none

type(testsuite_type), allocatable :: testsuites(:)
integer :: stat, is

stat = 0
testsuites = [ &
    new_testsuite("lexer", collect_lexer), &
    new_testsuite("structure", collect_structure), &
    new_testsuite("layout", collect_layout), &
    new_testsuite("whitespace", collect_whitespace), &
    new_testsuite("case", collect_case), &
    new_testsuite("split", collect_split), &
    new_testsuite("tools", collect_tools) &
    ]
do is = 1, size(testsuites)
    write (error_unit, '(a)') '# Testing: '//testsuites(is)%name
    call run_testsuite(testsuites(is)%collect, error_unit, stat)
end do
if (stat > 0) then
    write (error_unit, '(i0, a)') stat, ' test(s) failed!'
    error stop
end if
end program tester
