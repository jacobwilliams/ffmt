module test_layout
!! Tests of indentation and continuation layout through [[format_text]].
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_format
use ffmt_options
implicit none
private

public :: collect_layout

character, parameter :: NL = achar(10)

contains

subroutine collect_layout(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("indentation", test_indentation), &
        new_unittest("program unit bodies", test_units), &
        new_unittest("aligned continuation lines", test_align), &
        new_unittest("indented continuation lines", test_cont_indent), &
        new_unittest("comments", test_comments), &
        new_unittest("labels and semicolons", test_labels), &
        new_unittest("directives", test_directives), &
        new_unittest("preprocessor and strings", test_preprocessor_strings), &
        new_unittest("line endings and blanks", test_line_endings), &
        new_unittest("line length", test_line_length), &
        new_unittest("base indent and column-1 comments", test_base_indent), &
        new_unittest("procedure bodies", test_procedure_bodies), &
        new_unittest("comments after statements", test_comment_offsets), &
        new_unittest("idempotence", test_idempotence), &
        new_unittest("same_code", test_same_code), &
        new_unittest("comment spacing", test_comment_spacing), &
        new_unittest("fypp blocks", test_fypp_blocks) &
        ]
end subroutine collect_layout

function lines(text) result(s)
    !! Join `text` items with line feeds (a trailing line feed is added).
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
    type(format_options_t), intent(in), optional :: opts
    type(format_options_t) :: o
    character(len=:), allocatable :: out, errmsg
    integer :: stat

    if (present(opts)) o = opts
    call format_text(input, o, out, stat, errmsg)
    call check(error, stat, FMT_OK)
    if (allocated(error)) return
    call check(error, out == expected .and. len(out) == len(expected), &
               'formatted:'//NL//out//'expected:'//NL//expected)
end subroutine expect

subroutine test_indentation(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, lines([character(len=40) :: &
                              'subroutine s(x)', &
                              'integer :: x', &
                              '  if (x > 0) then', &
                              'do i = 1, x', &
                              '  call f(i)', &
                              'end do', &
                              '        else', &
                              'x = 0', &
                              'end if', &
                              'end subroutine']), &
                lines([character(len=40) :: &
                       'subroutine s(x)', &
                       '    integer :: x', &
                       '    if (x > 0) then', &
                       '        do i = 1, x', &
                       '            call f(i)', &
                       '        end do', &
                       '    else', &
                       '        x = 0', &
                       '    end if', &
                       'end subroutine']))
end subroutine test_indentation

subroutine test_units(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: src

    src = lines([character(len=40) :: 'module m', 'integer :: n', 'contains', 'subroutine s', &
                 'n = 1', 'end subroutine', 'end module'])
    call expect(error, src, lines([character(len=40) :: 'module m', 'integer :: n', 'contains', &
                                   'subroutine s', '    n = 1', 'end subroutine', 'end module']))
    if (allocated(error)) return
    opts%indent_units = .true.
    opts%indent = 2
    call expect(error, src, lines([character(len=40) :: 'module m', '  integer :: n', 'contains', &
                                   '  subroutine s', '    n = 1', '  end subroutine', 'end module']), opts)
end subroutine test_units

subroutine test_align(error)
    type(error_type), allocatable, intent(out) :: error
    ! first item after the innermost open bracket
    call expect(error, lines([character(len=50) :: &
                              'call foo(a, b, &', &
                              '  c, bar(d, &', &
                              '  e), &', &
                              '  f)']), &
                lines([character(len=50) :: &
                       'call foo(a, b, &', &
                       '         c, bar(d, &', &
                       '                e), &', &
                       '         f)']))
    if (allocated(error)) return
    ! after the first top-level `=`, and with leading `&`
    call expect(error, lines([character(len=50) :: &
                              'x = a + &', &
                              '     & b + &', &
                              '&c']), &
                lines([character(len=50) :: &
                       'x = a + &', &
                       '  & b + &', &
                       '   &c']))
    if (allocated(error)) return
    ! after a declaration's `::` and a `use` statement's `only:`, not after a rename `=>`
    call expect(error, lines([character(len=70) :: &
                              'integer, intent(in) :: a, &', &
                              'b', &
                              'use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &', &
                              'stderr => error_unit']), &
                lines([character(len=70) :: &
                       'integer, intent(in) :: a, &', &
                       '                       b', &
                       'use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &', &
                       '                                         stderr => error_unit']))
    if (allocated(error)) return
    ! nothing to align with: fixed indent
    call expect(error, lines([character(len=50) :: &
                              'call foo( &', &
                              'a)', &
                              'print *, a, &', &
                              'b']), &
                lines([character(len=50) :: &
                       'call foo( &', &
                       '    a)', &
                       'print *, a, &', &
                       '    b']))
end subroutine test_align

subroutine test_cont_indent(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%continuation = CONT_INDENT
    opts%continuation_indent = 2
    call expect(error, lines([character(len=50) :: 'subroutine s', 'call foo(a, &', 'b)', 'end subroutine']), &
                lines([character(len=50) :: 'subroutine s', '    call foo(a, &', '      b)', 'end subroutine']), opts)
end subroutine test_cont_indent

subroutine test_comments(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: src

    src = lines([character(len=40) :: 'do i = 1, 2', '! inside   ', 'x = f(1, & ! note', &
                 '! between', '2)', 'end do'])
    call expect(error, src, lines([character(len=40) :: 'do i = 1, 2', '    ! inside', &
                                   '    x = f(1, & ! note', '        ! between', '          2)', 'end do']))
    if (allocated(error)) return
    ! comment lines continuing a trailing comment stay lined up with it
    call expect(error, lines([character(len=60) :: &
                              'subroutine s(kx)', &
                              'integer :: kx  !! order of the spline', &
                              '               !! (degree + 1)', &
                              '               !! more', &
                              '! a separate comment', &
                              'end subroutine']), &
                lines([character(len=60) :: &
                       'subroutine s(kx)', &
                       '    integer :: kx  !! order of the spline', &
                       '                   !! (degree + 1)', &
                       '                   !! more', &
                       '    ! a separate comment', &
                       'end subroutine']))
    if (allocated(error)) return
    opts%indent_comments = .false.
    call expect(error, lines([character(len=40) :: 'do i = 1, 2', '! keep', 'end do']), &
                lines([character(len=40) :: 'do i = 1, 2', '! keep', 'end do']), opts)
end subroutine test_comments

subroutine test_labels(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, lines([character(len=40) :: 'do 10 i = 1, 2', '   10 x = x + 1', &
                              'if (x) then; y = 1', 'end if']), &
                lines([character(len=40) :: 'do 10 i = 1, 2', '10  x = x + 1', &
                       'if (x) then; y = 1', 'end if']))
    if (allocated(error)) return
    ! a label longer than the indentation keeps one blank before the statement
    call expect(error, lines([character(len=40) :: 'do i = 1, 2', 'do j = 1, 2', '12345 continue', &
                              'end do', 'end do']), &
                lines([character(len=40) :: 'do i = 1, 2', '    do j = 1, 2', '12345   continue', &
                       '    end do', 'end do']))
end subroutine test_labels

subroutine test_directives(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, lines([character(len=40) :: 'do i = 1, 2', 'x  =  1  !&', '!&<', &
                              '  y =   2', '!&>', 'z = 3', 'end do']), &
                lines([character(len=40) :: 'do i = 1, 2', 'x  =  1  !&', '!&<', &
                       '  y =   2', '!&>', '    z = 3', 'end do']))
end subroutine test_directives

subroutine test_preprocessor_strings(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: src
    ! preprocessor lines and the inside of continued strings are left alone
    src = 'subroutine s'//NL//'#ifdef X'//NL//'call f(1)'//NL//'#endif'//NL// &
          "s = 'abc&"//NL//'  def'''//NL//'end subroutine'//NL
    call expect(error, src, 'subroutine s'//NL//'#ifdef X'//NL//'    call f(1)'//NL//'#endif'//NL// &
                "    s = 'abc&"//NL//'  def'''//NL//'end subroutine'//NL)
end subroutine test_preprocessor_strings

subroutine test_line_endings(error)
    type(error_type), allocatable, intent(out) :: error
    character, parameter :: CR = achar(13)
    ! CRLF is kept, trailing blanks and blank-line blanks are removed, no newline is added at the end
    call expect(error, 'do i = 1, 2'//CR//NL//'   '//CR//NL//'x = 1   '//CR//NL//'end do  ', &
                'do i = 1, 2'//CR//NL//CR//NL//'    x = 1'//CR//NL//'end do')
end subroutine test_line_endings

subroutine test_base_indent(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%base_indent = 4
    opts%keep_column1_comments = .true.
    ! everything is indented, program units included, except comments in column 1
    call expect(error, lines([character(len=40) :: '!****', 'module m', 'implicit none', 'contains', &
                              '!> doc', 'subroutine s', '  ! inside', 'x = 1', 'end subroutine', 'end module']), &
                lines([character(len=40) :: '!****', '    module m', '    implicit none', '    contains', &
                       '!> doc', '    subroutine s', '        ! inside', '        x = 1', '    end subroutine', &
                       '    end module']), opts)
end subroutine test_base_indent

subroutine test_procedure_bodies(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%indent_procedures = .false.
    ! procedure bodies are not indented, but interface bodies still are
    call expect(error, lines([character(len=40) :: 'subroutine s', 'if (x) then', 'y = 1', 'end if', &
                              'interface', 'function f(x)', 'real :: x', 'end function', 'end interface', &
                              'end subroutine']), &
                lines([character(len=40) :: 'subroutine s', 'if (x) then', '    y = 1', 'end if', &
                       'interface', '    function f(x)', '        real :: x', '    end function', 'end interface', &
                       'end subroutine']), opts)
end subroutine test_procedure_bodies

subroutine test_comment_offsets(error)
    type(error_type), allocatable, intent(out) :: error
    ! documentation indented under the statement it follows keeps its extra indentation; a comment
    ! less indented than the code around it is re-indented
    call expect(error, lines([character(len=60) :: &
                              'subroutine s', &
                              '  character(len=*), parameter :: v = "1"', &
                              '      !! the version', &
                              '      !! (see the docs)', &
                              '  if (x) then', &
                              '   ! less than a level', &
                              '  end if', &
                              'end subroutine']), &
                lines([character(len=60) :: &
                       'subroutine s', &
                       '    character(len=*), parameter :: v = "1"', &
                       '        !! the version', &
                       '        !! (see the docs)', &
                       '    if (x) then', &
                       '        ! less than a level', &
                       '    end if', &
                       'end subroutine']))
    if (allocated(error)) return
    ! `generic` bindings are aligned after `=>`
    call expect(error, lines([character(len=60) :: &
                              'generic, public :: get => get_a, &', &
                              'get_b']), &
                lines([character(len=60) :: &
                       'generic, public :: get => get_a, &', &
                       '                          get_b']))
end subroutine test_comment_offsets

subroutine test_line_length(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: out, errmsg
    type(diagnostic_t), allocatable :: diags(:)
    integer :: stat

    opts%line_length = 30
    ! aligning `bbbbbbbbbbbb` under `aaaa` would exceed 30 columns: use the fixed indent instead
    call expect(error, lines([character(len=50) :: &
                              'call some_long_name(aaaa, &', &
                              'bbbbbbbbbbbbbbbbb)']), &
                lines([character(len=50) :: &
                       'call some_long_name(aaaa, &', &
                       '    bbbbbbbbbbbbbbbbb)']), opts)
    if (allocated(error)) return
    ! without line splitting, nesting that pushes a line past the limit keeps its original
    ! indentation, with a warning
    opts%split_lines = .false.
    call format_text(lines([character(len=50) :: 'do i = 1, 2', 'x = 12345678901234567890123', &
                            'end do']), opts, out, stat, errmsg, diags)
    call check(error, out, lines([character(len=50) :: 'do i = 1, 2', 'x = 12345678901234567890123', &
                                  'end do']))
    if (allocated(error)) return
    call check(error, size(diags) == 1, 'one diagnostic')
    if (allocated(error)) return
    call check(error, diags(1)%line, 2)
end subroutine test_line_length

subroutine test_idempotence(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: src, once, twice, errmsg
    integer :: stat

    src = lines([character(len=50) :: 'module m', 'contains', 'subroutine s(a, &', 'b)', &
                 'x = foo(a, &', '& b) + &', 'c', 'if (a) then', '! c', 'y = [1, &', '2]', 'end if', &
                 'end subroutine', 'end module'])
    call format_text(src, opts, once, stat, errmsg)
    call format_text(once, opts, twice, stat, errmsg)
    call check(error, twice == once .and. len(twice) == len(once), 'second pass changed:'//NL//once//NL//twice)
end subroutine test_idempotence

subroutine test_same_code(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, same_code('x  =  1 ! a'//NL, 'x=1'))
    if (allocated(error)) return
    call check(error, .not. same_code('x = 1', 'x = 2'))
    if (allocated(error)) return
    call check(error, .not. same_code('x = 1', 'x = 1; y'))
    if (allocated(error)) return
    ! a token split across lines must stay joined
    call check(error, .not. same_code('fun&'//NL//'&ction', 'fun&'//NL//'& ction'))
    if (allocated(error)) return
    call check(error, same_code('IF (A .LT. B) X = 1', 'if (a<b) x=1'), 'case and relational spelling')
    if (allocated(error)) return
    call check(error, .not. same_code('a .le. b', 'a < b'), 'different relational operators')
    if (allocated(error)) return
    call check(error, .not. same_code("s = 'A'", "s = 'a'"), 'case inside a string')
    if (allocated(error)) return
    call check(error, .not. same_code('x = 1', 'x = 1_8'), 'kind suffix')
end subroutine test_same_code

subroutine test_comment_spacing(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    opts%comment_spacing = 1
    call expect(error, lines([character(len=40) :: 'x = 1      ! far', 'y = 2! touching', 's = "a ! b"']), &
                lines([character(len=40) :: 'x = 1 ! far', 'y = 2 ! touching', 's = "a ! b"']), opts)
    if (allocated(error)) return
    ! comments lined up in a column stay; a comment line under a trailing comment moves with it
    call expect(error, lines([character(len=40) :: 'a = 1   ! one', 'bb = 2  ! two', 'c = 3    ! three', &
                              '         ! more']), &
                lines([character(len=40) :: 'a = 1   ! one', 'bb = 2  ! two', 'c = 3 ! three', '      ! more']), opts)
    if (allocated(error)) return
    ! after a continuation marker
    call expect(error, lines([character(len=40) :: 'call f(a, &     ! first', '       b)']), &
                lines([character(len=40) :: 'call f(a, & ! first', '       b)']), opts)
    if (allocated(error)) return
    ! lined up in the input, but not once the code is formatted: spaced, and stable
    call expect(error, lines([character(len=40) :: 'x=1     ! a', 'yy = 2  ! b']), &
                lines([character(len=40) :: 'x = 1 ! a', 'yy = 2 ! b']), opts)
end subroutine test_comment_spacing

subroutine test_fypp_blocks(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    character(len=:), allocatable :: src, want
    src = lines([character(len=40) :: 'subroutine s()', '#:if DEBUG', '#:for k in [1, 2]', 'print *, ${k}$', &
                 '#:endfor', '#:else', 'x = 0', '#:endif', '#ifdef CPP', 'y = 1', '#endif', 'end subroutine'])
    ! not indented by default
    call expect(error, src, lines([character(len=40) :: 'subroutine s()', '#:if DEBUG', '#:for k in [1, 2]', &
                                   '    print *, ${k}$', '#:endfor', '#:else', '    x = 0', '#:endif', '#ifdef CPP', &
                                   '    y = 1', '#endif', 'end subroutine']))
    if (allocated(error)) return
    opts%indent_fypp = .true.
    want = lines([character(len=40) :: 'subroutine s()', '    #:if DEBUG', '        #:for k in [1, 2]', &
                  '            print *, ${k}$', '        #:endfor', '    #:else', '        x = 0', '    #:endif', &
                  '#ifdef CPP', '    y = 1', '#endif', 'end subroutine'])
    call expect(error, src, want, opts)
    if (allocated(error)) return
    call expect(error, want, want, opts)                ! stable
end subroutine test_fypp_blocks

end module test_layout
