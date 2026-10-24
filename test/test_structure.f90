module test_structure
!! Tests of statement grouping, classification and block structure.
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_tokens
use ffmt_lexer, only: tokenize
use ffmt_statements
use ffmt_constructs
use ffmt_structure
use ffmt_source, only: source_t, parse_source, statement_views
implicit none
private

public :: collect_structure

character, parameter :: NL = achar(10)

contains

subroutine collect_structure(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("statement grouping", test_grouping), &
        new_unittest("semicolons", test_semicolons), &
        new_unittest("classification", test_classification), &
        new_unittest("depths", test_depths), &
        new_unittest("label-terminated do", test_label_do), &
        new_unittest("preprocessor branches", test_preprocessor_branches), &
        new_unittest("main program without program statement", test_implicit_program), &
        new_unittest("diagnostics", test_diagnostics), &
        new_unittest("constructs closed by end", test_closed_construct) &
        ]
end subroutine collect_structure

function item_kinds(src) result(kinds)
    !! Kinds of the items of `src`, as a string such as "CBM" (code, blank, comment, preprocessor).
    character(len=*), intent(in) :: src
    character(len=:), allocatable :: kinds
    type(token_list_t) :: tokens
    type(statement_list_t) :: stmts
    integer :: k
    character(len=*), parameter :: letters = 'CBMP'

    call tokenize(src, tokens)
    call split_statements(src, tokens, stmts)
    kinds = ''
    do k = 1, stmts%n
        kinds = kinds//letters(stmts%s(k)%kind:stmts%s(k)%kind)
    end do
end function item_kinds

subroutine test_grouping(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: src
    type(token_list_t) :: tokens
    type(statement_list_t) :: stmts

    src = 'a = 1'//NL//NL//'! note'//NL//'#if X'//NL//'b = 2 + &'//NL//'! inside'//NL//NL//'#endif'//NL// &
          '  & 3'//NL//'c = 4'
    call check(error, item_kinds(src), 'CBMPCC')
    if (allocated(error)) return
    ! the continued statement takes in the comment, blank and preprocessor lines
    call tokenize(src, tokens)
    call split_statements(src, tokens, stmts)
    call check(error, tokens%tok(stmts%s(5)%last)%line, 9)
    if (allocated(error)) return
    ! items cover all tokens, in order
    call check(error, stmts%s(1)%first == 1 .and. stmts%s(stmts%n)%last == tokens%n)
    if (allocated(error)) return
    call check(error, all(stmts%s(2:stmts%n)%first == stmts%s(1:stmts%n - 1)%last + 1))
    if (allocated(error)) return
    ! a continuation at the end of the file
    call check(error, item_kinds('a = &'), 'C')
end subroutine test_grouping

subroutine test_semicolons(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, item_kinds('a = 1; b = 2; c = 3'), 'CCC')
    if (allocated(error)) return
    call check(error, item_kinds('a = 1;  ! trailing'//NL//'b = 2'), 'CC')
    if (allocated(error)) return
    call check(error, item_kinds("s = 'x;y'; t = &"//NL//'  1; u = 2'), 'CCC')
end subroutine test_semicolons

subroutine expect_class(error, src, role, construct, in_interface)
    !! Role and construct of the first statement of `src`.
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: src
    integer, intent(in) :: role, construct
    logical, intent(in), optional :: in_interface
    type(token_list_t) :: tokens
    type(statement_list_t) :: stmts
    type(stmt_info_t) :: info
    logical :: inside

    inside = .false.
    if (present(in_interface)) inside = in_interface
    call tokenize(src, tokens)
    call split_statements(src, tokens, stmts)
    info = classify_statement(src, tokens, statement_view(src, tokens, stmts%s(1)), inside)
    call check(error, info%role == role .and. info%construct == construct, &
               '"'//src//'": got role '//to_s(info%role)//' construct '//construct_label(info%construct)// &
               ', expected role '//to_s(role)//' construct '//construct_label(construct))
end subroutine expect_class

subroutine test_classification(error)
    type(error_type), allocatable, intent(out) :: error
    integer :: k
    type :: case_t
        character(len=48) :: src
        integer :: role, construct
    end type case_t
    type(case_t), parameter :: cases(*) = [ &
        case_t('program main', ROLE_OPEN, C_PROGRAM), &
        case_t('module foo', ROLE_OPEN, C_MODULE), &
        case_t('submodule (a) b', ROLE_OPEN, C_SUBMODULE), &
        case_t('module procedure foo', ROLE_OPEN, C_PROCEDURE), &
        case_t('module function f(x) result(y)', ROLE_OPEN, C_FUNCTION), &
        case_t('subroutine s(a, b)', ROLE_OPEN, C_SUBROUTINE), &
        case_t('pure recursive subroutine s', ROLE_OPEN, C_SUBROUTINE), &
        case_t('elemental real(dp) function f(x)', ROLE_OPEN, C_FUNCTION), &
        case_t('character(len=*) function f()', ROLE_OPEN, C_FUNCTION), &
        case_t('integer*8 function f()', ROLE_OPEN, C_FUNCTION), &
        case_t('type(point) function f()', ROLE_OPEN, C_FUNCTION), &
        case_t('double precision function f()', ROLE_OPEN, C_FUNCTION), &
        case_t('real :: function', ROLE_NONE, C_NONE), &
        case_t('real function_value', ROLE_NONE, C_NONE), &
        case_t('block data init', ROLE_OPEN, C_BLOCK_DATA), &
        case_t('blockdata', ROLE_OPEN, C_BLOCK_DATA), &
        case_t('abstract interface', ROLE_OPEN, C_INTERFACE), &
        case_t('interface operator(+)', ROLE_OPEN, C_INTERFACE), &
        case_t('type :: point', ROLE_OPEN, C_TYPE), &
        case_t('type, extends(base), abstract :: t', ROLE_OPEN, C_TYPE), &
        case_t('type point', ROLE_OPEN, C_TYPE), &
        case_t('type(point) :: p', ROLE_NONE, C_NONE), &
        case_t('enum, bind(c)', ROLE_OPEN, C_ENUM), &
        case_t('if (a > b) then', ROLE_OPEN, C_IF), &
        case_t('outer: if (x) then', ROLE_OPEN, C_IF), &
        case_t('get_${k}$: if (x) then', ROLE_OPEN, C_IF), &
        case_t('if (x) y = 1', ROLE_NONE, C_NONE), &
        case_t('if (x) then = 1', ROLE_NONE, C_NONE), &
        case_t('else if (x) then', ROLE_MIDDLE, C_IF), &
        case_t('elseif(x)then', ROLE_MIDDLE, C_IF), &
        case_t('else', ROLE_MIDDLE, C_IF), &
        case_t('else outer', ROLE_MIDDLE, C_IF), &
        case_t('endif', ROLE_CLOSE, C_IF), &
        case_t('end if outer', ROLE_CLOSE, C_IF), &
        case_t('do', ROLE_OPEN, C_DO), &
        case_t('do i = 1, n', ROLE_OPEN, C_DO), &
        case_t('do while (x)', ROLE_OPEN, C_DO), &
        case_t('do concurrent (i = 1:n)', ROLE_OPEN, C_DO), &
        case_t('enddo', ROLE_CLOSE, C_DO), &
        case_t('do = 1', ROLE_NONE, C_NONE), &
        case_t('select case (x)', ROLE_OPEN, C_SELECT), &
        case_t('selecttype (p => q)', ROLE_OPEN, C_SELECT), &
        case_t('select rank (a)', ROLE_OPEN, C_SELECT), &
        case_t('case (1:3)', ROLE_MIDDLE, C_SELECT), &
        case_t('case default', ROLE_MIDDLE, C_SELECT), &
        case_t('type is (integer)', ROLE_MIDDLE, C_SELECT), &
        case_t('class is (t)', ROLE_MIDDLE, C_SELECT), &
        case_t('class default', ROLE_MIDDLE, C_SELECT), &
        case_t('class(t), intent(in) :: x', ROLE_NONE, C_NONE), &
        case_t('rank (*)', ROLE_MIDDLE, C_SELECT), &
        case_t('end select', ROLE_CLOSE, C_SELECT), &
        case_t('associate (a => b%c)', ROLE_OPEN, C_ASSOCIATE), &
        case_t('block', ROLE_OPEN, C_BLOCK), &
        case_t('endblock', ROLE_CLOSE, C_BLOCK), &
        case_t('critical', ROLE_OPEN, C_CRITICAL), &
        case_t('change team (t)', ROLE_OPEN, C_TEAM), &
        case_t('end team', ROLE_CLOSE, C_TEAM), &
        case_t('where (m > 0)', ROLE_OPEN, C_WHERE), &
        case_t('where (m > 0) a = 1', ROLE_NONE, C_NONE), &
        case_t('elsewhere (m < 0)', ROLE_MIDDLE, C_WHERE), &
        case_t('else where', ROLE_MIDDLE, C_WHERE), &
        case_t('forall (i = 1:n)', ROLE_OPEN, C_FORALL), &
        case_t('forall (i = 1:n) a(i) = i', ROLE_NONE, C_NONE), &
        case_t('contains', ROLE_MIDDLE, C_CONTAINS), &
        case_t('end', ROLE_CLOSE, C_ANY), &
        case_t('end subroutine s', ROLE_CLOSE, C_SUBROUTINE), &
        case_t('endprocedure', ROLE_CLOSE, C_PROCEDURE), &
        case_t('end block data', ROLE_CLOSE, C_BLOCK_DATA), &
        case_t('END BLOCKDATA', ROLE_CLOSE, C_BLOCK_DATA), &
        case_t('module m_${k}$', ROLE_OPEN, C_MODULE), &
        case_t('pure module function ${name}$(x)', ROLE_OPEN, C_FUNCTION), &
        case_t('${t}$ function f(x)', ROLE_OPEN, C_FUNCTION), &
        case_t('type, deferred :: t', ROLE_NONE, C_NONE), &
        case_t('endfile (10)', ROLE_NONE, C_NONE), &
        case_t('end = 1', ROLE_NONE, C_NONE), &
        case_t('type%x = 1', ROLE_NONE, C_NONE), &
        case_t('10 continue', ROLE_NONE, C_NONE), &
        case_t('call do_it()', ROLE_NONE, C_NONE), &
        case_t('procedure, pass :: p', ROLE_NONE, C_NONE), &
        ! a procedure prefix made by a cpp macro
        case_t('PURE_ARRAY_EQ FUNCTION f(a, b)', ROLE_OPEN, C_FUNCTION), &
        case_t('MAYBE_PURE subroutine s', ROLE_OPEN, C_SUBROUTINE), &
        case_t('call function(x)', ROLE_NONE, C_NONE), &
        ! assignments to (elements of) variables named like keywords
        case_t('rank (1) = n', ROLE_NONE, C_NONE), &
        case_t('case(2)%x = 1', ROLE_NONE, C_NONE), &
        case_t('rank (1)', ROLE_MIDDLE, C_SELECT), &
        case_t('if (a(1) == 2) x = 1', ROLE_NONE, C_NONE) &
        ]

    do k = 1, size(cases)
        call expect_class(error, trim(cases(k)%src), cases(k)%role, cases(k)%construct)
        if (allocated(error)) return
    end do
    ! inside an interface block, `module procedure` names procedures of a generic interface
    call expect_class(error, 'module procedure foo', ROLE_NONE, C_NONE, in_interface=.true.)
    if (allocated(error)) return
    ! keywords split across a continuation line
    call expect_class(error, 'end &'//NL//'  & subroutine', ROLE_CLOSE, C_SUBROUTINE)
end subroutine test_classification

function depths(src) result(d)
    !! Depth of each item of `src`.
    character(len=*), intent(in) :: src
    integer, allocatable :: d(:)
    type(source_t) :: source
    type(structure_t) :: st
    type(diagnostic_t), allocatable :: diags(:)
    call parse_source(src, source)
    call analyze_structure(source, statement_views(source), st, diags)
    d = st%depth
end function depths

subroutine test_depths(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: src

    src = 'module m'//NL// &
          'contains'//NL// &
          'subroutine s(x)'//NL// &
          'if (x) then'//NL// &
          'y = 1'//NL// &
          'else'//NL// &
          '! comment'//NL// &
          'y = 2'//NL// &
          'end if'//NL// &
          'end subroutine'//NL// &
          'end module'
    call check(error, all(depths(src) == [0, 0, 1, 2, 3, 2, 3, 3, 2, 1, 0]), 'depths')
end subroutine test_depths

subroutine test_label_do(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: src

    src = 'do 10 i = 1, n'//NL// &
          'do 10 j = 1, n'//NL// &
          'a(i,j) = 0'//NL// &
          '10 continue'//NL// &
          'b = 1'
    call check(error, all(depths(src) == [0, 1, 2, 2, 0]), 'depths')
end subroutine test_label_do

subroutine test_preprocessor_branches(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: src

    ! each branch opens the subroutine; the structure continues from the last branch
    src = '#ifdef A'//NL// &
          'subroutine s(x)'//NL// &
          '#else'//NL// &
          'subroutine s()'//NL// &
          '#endif'//NL// &
          'y = 1'//NL// &
          'end subroutine'
    call check(error, all(depths(src) == [0, 0, 0, 0, 1, 1, 0]), 'cpp depths')
    if (allocated(error)) return
    src = 'subroutine s'//NL// &
          '#:if k == 1'//NL// &
          'if (a) then'//NL// &
          '#:else'//NL// &
          'if (b) then'//NL// &
          '#:endif'//NL// &
          'y = 1'//NL// &
          'end if'//NL// &
          'end subroutine'
    call check(error, all(depths(src) == [0, 1, 1, 1, 1, 2, 2, 1, 0]), 'fypp depths')
    if (allocated(error)) return
    ! a construct opened and closed under the same condition
    src = '#ifdef A'//NL// &
          'do i = 1, 2'//NL// &
          '#endif'//NL// &
          'y = 1'//NL// &
          '#ifdef A'//NL// &
          'end do'//NL// &
          '#endif'
    call check(error, all(depths(src) == [0, 0, 1, 1, 1, 0, 0]), 'conditional do depths')
end subroutine test_preprocessor_branches

subroutine test_implicit_program(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: src
    type(source_t) :: source
    type(structure_t) :: st
    integer, allocatable :: d(:)
    type(diagnostic_t), allocatable :: diags(:)

    ! a main program may omit the `program` statement
    src = achar(239)//achar(187)//achar(191)//'x = 1'//NL//'contains'//NL//'subroutine s'//NL// &
          'end subroutine'//NL//'end program'
    call parse_source(src, source)
    call analyze_structure(source, statement_views(source), st, diags)
    d = st%depth
    call check(error, size(diags), 0)
    if (allocated(error)) return
    call check(error, all(d == [0, 0, 1, 1, 0]), 'depths')
end subroutine test_implicit_program

subroutine test_diagnostics(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: src
    type(source_t) :: source
    type(structure_t) :: st
    integer, allocatable :: d(:)
    type(diagnostic_t), allocatable :: diags(:)

    src = 'subroutine s'//NL//'do i = 1, 2'//NL//'end subroutine'//NL//'else'//NL//'end do'//NL//'if (x) then'
    call parse_source(src, source)
    call analyze_structure(source, statement_views(source), st, diags)
    d = st%depth
    call check(error, size(diags), 4)
    if (allocated(error)) return
    call check(error, diags(1)%message, 'end subroutine closes do (opened on line 2)')
    if (allocated(error)) return
    call check(error, diags(2)%message, 'if statement outside any construct')
    if (allocated(error)) return
    call check(error, diags(3)%message, 'end do without an open construct')
    if (allocated(error)) return
    call check(error, diags(4)%line == 6 .and. diags(4)%message == 'if is not closed')
end subroutine test_diagnostics

pure function to_s(i) result(s)
    integer, intent(in) :: i
    character(len=:), allocatable :: s
    character(len=16) :: buf
    write (buf, '(i0)') i
    s = trim(buf)
end function to_s

subroutine test_closed_construct(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, closed_construct('', '') == C_ANY, 'bare end')
    if (allocated(error)) return
    call check(error, closed_construct('do', '') == C_DO, 'end do')
    if (allocated(error)) return
    call check(error, closed_construct('block', 'data') == C_BLOCK_DATA .and. &
               closed_construct('block', '') == C_BLOCK, 'end block [data]')
    if (allocated(error)) return
    call check(error, closed_construct('x', '') == C_NONE, 'end followed by a name')
    if (allocated(error)) return
    call check(error, closed_construct('enumeration', 'type') == C_ENUMERATION, 'end enumeration type')
    if (allocated(error)) return
    call check(error, all(depths('enumeration type, bind(c) :: color'//NL//'enumerator :: red'//NL// &
                                 'end enumeration type') == [0, 1, 0]), 'enumeration type depths')
end subroutine test_closed_construct

end module test_structure
