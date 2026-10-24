module test_api
!! Tests of the fregex API: spans, groups, flags, find_all, split, sub, errors and limits.
use iso_fortran_env, only: int64
use testdrive, only: new_unittest, unittest_type, error_type, check
use fregex
implicit none
private

public :: collect_api

contains

subroutine collect_api(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("search spans", test_search_spans), &
        new_unittest("groups", test_groups), &
        new_unittest("named groups", test_named_groups), &
        new_unittest("match and fullmatch", test_match_modes), &
        new_unittest("search from position", test_pos), &
        new_unittest("flags", test_flags), &
        new_unittest("variable-width lookbehind", test_lookbehind), &
        new_unittest("find_all", test_find_all), &
        new_unittest("split", test_split), &
        new_unittest("sub with template", test_sub_template), &
        new_unittest("sub with callback", test_sub_callback), &
        new_unittest("escape", test_escape), &
        new_unittest("syntax errors", test_syntax_errors), &
        new_unittest("not compiled", test_not_compiled), &
        new_unittest("step limit", test_step_limit), &
        new_unittest("long subject", test_long_subject) &
        ]
end subroutine collect_api

subroutine test_search_spans(error)
    type(error_type), allocatable, intent(out) :: error
    type(match_t) :: m

    call check(error, re_search('b+', 'abbbc', m))
    if (allocated(error)) return
    call check(error, m%start(), 2)
    if (allocated(error)) return
    call check(error, m%end(), 4)
    if (allocated(error)) return
    call check(error, m%group(), 'bbb')
    if (allocated(error)) return
    ! an empty match has end = start - 1
    call check(error, re_search('x*', 'abc', m))
    if (allocated(error)) return
    call check(error, m%start() == 1 .and. m%end() == 0 .and. m%group() == '')
    if (allocated(error)) return
    call check(error, .not. re_search('z', 'abc'))
end subroutine test_search_spans

subroutine test_groups(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re
    type(match_t) :: m

    re = regex('(\w+)=(\d+)?(;)?')
    call check(error, re%group_count(), 3)
    if (allocated(error)) return
    call check(error, re%search('  key=;', m))
    if (allocated(error)) return
    call check(error, m%group_count(), 3)
    if (allocated(error)) return
    call check(error, m%group(1), 'key')
    if (allocated(error)) return
    call check(error, m%matched(1) .and. .not. m%matched(2) .and. m%matched(3))
    if (allocated(error)) return
    ! a group that did not match has start 0, end -1 and empty text
    call check(error, m%start(2) == 0 .and. m%end(2) == -1 .and. m%group(2) == '')
end subroutine test_groups

subroutine test_named_groups(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re
    type(match_t) :: m

    re = regex('(?P<name>\w+)\s*::\s*(?<rest>.*)')
    call check(error, re%group_index('rest'), 2)
    if (allocated(error)) return
    call check(error, re%group_index('nope'), 0)
    if (allocated(error)) return
    call check(error, re%search('integer :: i, j', m))
    if (allocated(error)) return
    call check(error, m%group('name'), 'integer')
    if (allocated(error)) return
    call check(error, m%group('rest'), 'i, j')
    if (allocated(error)) return
    call check(error, m%start('rest'), 12)
end subroutine test_named_groups

subroutine test_match_modes(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re

    re = regex('a+')
    call check(error, re%match('aab') .and. .not. re%match('baa'))
    if (allocated(error)) return
    call check(error, re%fullmatch('aaa') .and. .not. re%fullmatch('aab'))
    if (allocated(error)) return
    ! fullmatch backtracks to find a match that reaches the end
    call check(error, re_fullmatch('a|ab', 'ab'))
end subroutine test_match_modes

subroutine test_pos(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re
    type(match_t) :: m

    re = regex('\d')
    call check(error, re%search('1a2', m, pos=2))
    if (allocated(error)) return
    call check(error, m%start(), 3)
    if (allocated(error)) return
    ! '^' still means the start of the string, not the start position
    re = regex('^a')
    call check(error, .not. re%search('ba', pos=2))
    if (allocated(error)) return
    re = regex('a')
    call check(error, re%match('ba', pos=2) .and. .not. re%search('ba', pos=3))
end subroutine test_pos

subroutine test_flags(error)
    type(error_type), allocatable, intent(out) :: error
    type(match_t), allocatable :: ms(:)

    call check(error, re_search('end\s+do', 'END DO', flags=RE_IGNORECASE))
    if (allocated(error)) return
    call check(error, .not. re_search('end\s+do', 'END DO'))
    if (allocated(error)) return
    ms = re_find_all('^\w+', 'one'//new_line('a')//'two', flags=RE_MULTILINE)
    call check(error, size(ms), 2)
    if (allocated(error)) return
    call check(error, re_fullmatch('a.b', 'a'//new_line('a')//'b', flags=RE_DOTALL))
    if (allocated(error)) return
    call check(error, re_fullmatch('A.B', 'a'//new_line('a')//'b', flags=RE_DOTALL + RE_IGNORECASE))
end subroutine test_flags

subroutine test_lookbehind(error)
    type(error_type), allocatable, intent(out) :: error
    type(match_t) :: m

    ! Python rejects these; fregex allows lookbehind of any width
    call check(error, re_search('(?<=\bcall\s+)\w+', 'call  foo(x)', m))
    if (allocated(error)) return
    call check(error, m%group(), 'foo')
    if (allocated(error)) return
    call check(error, re_search('(?<!end\s*)do\b', 'enddo; do i = 1, 2', m))
    if (allocated(error)) return
    call check(error, m%start(), 8)
end subroutine test_lookbehind

subroutine test_find_all(error)
    type(error_type), allocatable, intent(out) :: error
    type(match_t), allocatable :: ms(:)

    ms = re_find_all('\d+', 'a1 b22 c333')
    call check(error, size(ms), 3)
    if (allocated(error)) return
    call check(error, ms(3)%group(), '333')
    if (allocated(error)) return
    call check(error, size(re_find_all('z', 'abc')), 0)
end subroutine test_find_all

subroutine test_split(error)
    type(error_type), allocatable, intent(out) :: error
    type(string_t), allocatable :: parts(:)

    parts = re_split('\s*,\s*', 'a , b,c')
    call check(error, size(parts), 3)
    if (allocated(error)) return
    call check(error, parts(1)%s == 'a' .and. parts(2)%s == 'b' .and. parts(3)%s == 'c')
    if (allocated(error)) return
    ! captured separators are included
    parts = re_split('(\+|-)', 'a+b-c')
    call check(error, size(parts), 5)
    if (allocated(error)) return
    call check(error, parts(2)%s == '+' .and. parts(4)%s == '-')
    if (allocated(error)) return
    parts = re_split(',', 'a,b,c', maxsplit=1)
    call check(error, size(parts) == 2 .and. parts(2)%s == 'b,c')
end subroutine test_split

subroutine test_sub_template(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re
    integer :: n

    call check(error, re_sub('(\w+)\s*=\s*(\w+)', 'a=b', '\2 = \1'), 'b = a')
    if (allocated(error)) return
    call check(error, re_sub('\s+', 'a   b    c', ' ', count=1), 'a b    c')
    if (allocated(error)) return
    re = regex('(?P<op>\.(?:eq|ne)\.)', RE_IGNORECASE)
    call check(error, re%sub('a.EQ.b.ne.c', ' \g<op> ', nsubs=n), 'a .EQ. b .ne. c')
    if (allocated(error)) return
    call check(error, n, 2)
end subroutine test_sub_template

function upper_word(m) result(text)
    type(match_t), intent(in) :: m
    character(len=:), allocatable :: text
    integer :: k
    text = m%group()
    do k = 1, len(text)
        if (text(k:k) >= 'a' .and. text(k:k) <= 'z') text(k:k) = achar(iachar(text(k:k)) - 32)
    end do
end function upper_word

subroutine test_sub_callback(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re

    re = regex('\b(?:end|do|if)\b', RE_IGNORECASE)
    call check(error, re%sub('if (x) then; do i=1,n; end do', upper_word), &
               'IF (x) then; DO i=1,n; END DO')
end subroutine test_sub_callback

subroutine test_escape(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, re_escape('a.b*(c)'), 'a\.b\*\(c\)')
    if (allocated(error)) return
    call check(error, re_fullmatch(re_escape('x+y=[1]'), 'x+y=[1]'))
end subroutine test_escape

subroutine test_syntax_errors(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re
    integer :: stat
    character(len=:), allocatable :: msg

    call re%compile('a(b', stat=stat, errmsg=msg)
    call check(error, stat, RE_ERR_SYNTAX)
    if (allocated(error)) return
    call check(error, msg, 'missing ), unterminated subpattern at position 2')
    if (allocated(error)) return
    call check(error, .not. re%is_compiled())
    if (allocated(error)) return
    call re%compile('a**', stat=stat, errmsg=msg)
    call check(error, msg, 'multiple repeat at position 3')
    if (allocated(error)) return
    re = regex('(a)')
    call check(error, re%sub('a', '\2', stat=stat), 'a')
    if (allocated(error)) return
    call check(error, stat, RE_ERR_TEMPLATE)
end subroutine test_syntax_errors

subroutine test_not_compiled(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re
    integer :: stat
    logical :: found
    found = re%search('abc', stat=stat)
    call check(error, stat == RE_ERR_NOT_COMPILED .and. .not. found)
end subroutine test_not_compiled

subroutine test_step_limit(error)
    type(error_type), allocatable, intent(out) :: error
    type(regex_t) :: re
    integer :: stat
    logical :: found

    ! exponential backtracking: every way of splitting the a's is tried before failing
    re = regex('(a+)+b')
    call re%set_step_limit(100000_int64)
    found = re%search(repeat('a', 40), stat=stat)
    call check(error, stat, RE_ERR_STEP_LIMIT)
    if (allocated(error)) return
    ! the possessive form fails fast
    re = regex('(a++)+b')
    call re%set_step_limit(100000_int64)
    found = re%search(repeat('a', 40), stat=stat)
    call check(error, stat == RE_OK .and. .not. found)
end subroutine test_step_limit

subroutine test_long_subject(error)
    type(error_type), allocatable, intent(out) :: error
    type(match_t) :: m
    character(len=:), allocatable :: s

    s = repeat('x', 200000)//'needle'//repeat('y', 1000)
    call check(error, re_search('ne+dle', s, m))
    if (allocated(error)) return
    call check(error, m%start(), 200001)
    if (allocated(error)) return
    call check(error, size(re_find_all('y', s)), 1000)
end subroutine test_long_subject

end module test_api
