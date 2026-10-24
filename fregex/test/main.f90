program tester
use iso_fortran_env, only: error_unit
use testdrive, only: run_testsuite, new_testsuite, testsuite_type
use test_api, only: collect_api
use test_conformance, only: collect_conformance
implicit none

type(testsuite_type), allocatable :: testsuites(:)
integer :: stat, is

stat = 0
testsuites = [ &
    new_testsuite("api", collect_api), &
    new_testsuite("conformance", collect_conformance) &
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
