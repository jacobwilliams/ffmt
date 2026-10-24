module test_lexer
!! Lexer tests. Expected token streams are written as `KIND:text` items separated by blanks,
!! with blanks (`_`) and line breaks (`NL`) shown without text.
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_tokens
use ffmt_lexer, only: tokenize
implicit none
private

public :: collect_lexer, describe

character, parameter :: NL = achar(10)

contains

subroutine collect_lexer(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("names, operators, numbers", test_basic), &
        new_unittest("real literals and dot operators", test_numbers), &
        new_unittest("strings", test_strings), &
        new_unittest("continued strings", test_continued_strings), &
        new_unittest("BOZ and kind prefixes", test_boz_and_kinds), &
        new_unittest("comments and directives", test_comments), &
        new_unittest("continuation lines", test_continuation), &
        new_unittest("split tokens are glued", test_glue), &
        new_unittest("preprocessor lines", test_preprocessor), &
        new_unittest("inline fypp", test_fypp_inline), &
        new_unittest("line endings and line numbers", test_line_endings), &
        new_unittest("round trip of odd input", test_round_trip) &
        ]
end subroutine collect_lexer

function describe(src) result(d)
    !! Describe the tokens of `src` as `KIND:text` items.
    character(len=*), intent(in) :: src
    character(len=:), allocatable :: d
    type(token_list_t) :: tokens
    integer :: k

    call tokenize(src, tokens)
    d = ''
    do k = 1, tokens%n
        associate (t => tokens%tok(k))
            if (k > 1) d = d//' '
            select case (t%kind)
            case (TK_SPACE)
                d = d//'_'
            case (TK_NEWLINE)
                d = d//'NL'
            case default
                d = d//kind_name(t%kind)//':'//src(t%first:t%last)
            end select
            if (t%glued) d = d//'^'
        end associate
    end do
end function describe

pure function kind_name(kind) result(name)
    integer, intent(in) :: kind
    character(len=:), allocatable :: name
    character(len=*), parameter :: names(*) = [character(len=7) :: 'SPACE', 'NL', 'COMMENT', 'CONT', &
                                               'PREPROC', 'NAME', 'INT', 'REAL', 'STRING', 'BOZ', 'DOTOP', 'OP', 'FYPP', 'OTHER']
    name = trim(names(kind))
end function kind_name

subroutine expect(error, src, expected)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: src, expected
    character(len=:), allocatable :: actual
    actual = describe(src)
    call check(error, actual == expected .and. len(actual) == len(expected), &
               'tokens of "'//src//'"'//NL//'  expected: '//expected//NL//'  actual:   '//actual)
end subroutine expect

subroutine test_basic(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, 'x = a**2 + b(:,1)', &
                'NAME:x _ OP:= _ NAME:a OP:** INT:2 _ OP:+ _ NAME:b OP:( OP:: OP:, INT:1 OP:)')
    if (allocated(error)) return
    call expect(error, 'integer, intent(in) :: n', &
                'NAME:integer OP:, _ NAME:intent OP:( NAME:in OP:) _ OP::: _ NAME:n')
    if (allocated(error)) return
    call expect(error, 'p=>q; if(a/=b.and.c<=d)s=s//t', &
                'NAME:p OP:=> NAME:q OP:; _ NAME:if OP:( NAME:a OP:/= NAME:b DOTOP:.and. NAME:c OP:<= NAME:d OP:) '// &
                'NAME:s OP:= NAME:s OP:// NAME:t')
    if (allocated(error)) return
    call expect(error, 'x(/1,2/) a%b c$d', &
                'NAME:x OP:( OP:/ INT:1 OP:, INT:2 OP:/ OP:) _ NAME:a OP:% NAME:b _ NAME:c$d')
end subroutine test_basic

subroutine test_numbers(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, '1 1.5 .5 1. 1e5 1.5d-3 2.e+1_dp 10_ik 3_8', &
                'INT:1 _ REAL:1.5 _ REAL:.5 _ REAL:1. _ REAL:1e5 _ REAL:1.5d-3 _ REAL:2.e+1_dp _ INT:10_ik _ INT:3_8')
    if (allocated(error)) return
    ! `1.eq.2` is 1 .eq. 2, not the real 1. followed by `eq.2`
    call expect(error, '1.eq.2 .true. .FALSE._lk .myop. x', &
                'INT:1 DOTOP:.eq. INT:2 _ DOTOP:.true. _ DOTOP:.FALSE._lk _ DOTOP:.myop. _ NAME:x')
    if (allocated(error)) return
    call expect(error, 'a.b 1e x', 'NAME:a OTHER:. NAME:b _ INT:1 NAME:e _ NAME:x')
end subroutine test_numbers

subroutine test_strings(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, "s = 'it''s' // ""a """"b"""" !"" ! c", &
                "NAME:s _ OP:= _ STRING:'it''s' _ OP:// _ STRING:""a """"b"""" !"" _ COMMENT:! c")
    if (allocated(error)) return
    call expect(error, "x = 'open"//NL//"y", "NAME:x _ OP:= _ STRING:'open NL NAME:y")
end subroutine test_strings

subroutine test_continued_strings(error)
    type(error_type), allocatable, intent(out) :: error
    type(token_list_t) :: tokens
    character(len=:), allocatable :: src

    src = "s = 'abc&"//NL//"    &def'"
    call tokenize(src, tokens)
    call check(error, tokens%n, 5)
    if (allocated(error)) return
    call check(error, token_text(src, tokens%tok(5)), "'abc&"//NL//"    &def'")
    if (allocated(error)) return
    call check(error, tokens%tok(5)%sub, STR_MULTILINE)
    if (allocated(error)) return

    ! without a leading `&` the blanks at the start of the next line belong to the string
    src = "s = 'abc&"//NL//"  def'"
    call tokenize(src, tokens)
    call check(error, tokens%n, 5)
    if (allocated(error)) return
    call check(error, tokens%tok(5)%sub, ior(STR_MULTILINE, STR_RAW_CONT))
    if (allocated(error)) return

    ! `&` followed by a comment is not a continuation inside a string
    call expect(error, "s = 'a & ! b'", "NAME:s _ OP:= _ STRING:'a & ! b'")
end subroutine test_continued_strings

subroutine test_boz_and_kinds(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, "b'0101' Z""ff"" o'17' c_char_'x' 1_'y' bz'1'", &
                "BOZ:b'0101' _ BOZ:Z""ff"" _ BOZ:o'17' _ STRING:c_char_'x' _ STRING:1_'y' _ NAME:bz STRING:'1'")
end subroutine test_boz_and_kinds

subroutine test_comments(error)
    type(error_type), allocatable, intent(out) :: error
    type(token_list_t) :: tokens
    character(len=:), allocatable :: src
    integer :: k
    integer, parameter :: expected(*) = [CM_PLAIN, CM_DIRECTIVE, CM_DIRECTIVE, CM_CONDITIONAL, CM_DIRECTIVE, &
                                         CM_FORMATTER, CM_FORMATTER]

    src = '! plain'//NL//'!$omp parallel'//NL//'!$ACC kernels'//NL//'!$ x = 1'//NL//'!DIR$ IVDEP'//NL// &
          '!&<'//NL//'!&'
    call tokenize(src, tokens)
    do k = 1, size(expected)
        call check(error, tokens%tok(2*k - 1)%kind == TK_COMMENT .and. tokens%tok(2*k - 1)%sub == expected(k), &
                   'comment kind of: '//token_text(src, tokens%tok(2*k - 1)))
        if (allocated(error)) return
    end do
end subroutine test_comments

subroutine test_continuation(error)
    type(error_type), allocatable, intent(out) :: error
    type(token_list_t) :: tokens
    character(len=:), allocatable :: src

    src = 'x = 1 + & ! c'//NL//'    & 2'
    call expect(error, src, 'NAME:x _ OP:= _ INT:1 _ OP:+ _ CONT:& _ COMMENT:! c NL _ CONT:& _ INT:2')
    if (allocated(error)) return
    call tokenize(src, tokens)
    call check(error, tokens%tok(9)%sub == CT_TRAILING .and. tokens%tok(14)%sub == CT_LEADING)
end subroutine test_continuation

subroutine test_glue(error)
    type(error_type), allocatable, intent(out) :: error
    ! a name split across lines: `fun&` + `&ction` is one token in Fortran
    call expect(error, 'fun&'//NL//'&ction', 'NAME:fun CONT:& NL CONT:& NAME:ction^')
    if (allocated(error)) return
    call expect(error, 'a &'//NL//'&b', 'NAME:a _ CONT:& NL CONT:& NAME:b')
    if (allocated(error)) return
    call expect(error, 'a&'//NL//'  b', 'NAME:a CONT:& NL _ NAME:b')
end subroutine test_glue

subroutine test_preprocessor(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, '#ifdef X'//NL//'  #define Y(a) \'//NL//'    a'//NL//'x', &
                'PREPROC:#ifdef X NL _ PREPROC:#define Y(a) \'//NL//'    a NL NAME:x')
    if (allocated(error)) return
    call expect(error, '#:if a &'//NL//'  & b'//NL//'$:x'//NL//'@:y', &
                'PREPROC:#:if a &'//NL//'  & b NL PREPROC:$:x NL PREPROC:@:y')
    if (allocated(error)) return
    ! `#` that is not at the start of a line is not a directive
    call expect(error, 'a # b', 'NAME:a _ OTHER:# _ NAME:b')
end subroutine test_preprocessor

subroutine test_fypp_inline(error)
    type(error_type), allocatable, intent(out) :: error
    call expect(error, 'x = ${a + b}$ + @{f(1)}@', 'NAME:x _ OP:= _ FYPP:${a + b}$ _ OP:+ _ FYPP:@{f(1)}@')
    if (allocated(error)) return
    call expect(error, '#{if x}# y', 'FYPP:#{if x}# _ NAME:y')
    if (allocated(error)) return
    call expect(error, 'f_${k}$_x a$b', 'NAME:f_ FYPP:${k}$ NAME:_x _ NAME:a$b')
end subroutine test_fypp_inline

subroutine test_line_endings(error)
    type(error_type), allocatable, intent(out) :: error
    type(token_list_t) :: tokens
    character(len=:), allocatable :: src
    character, parameter :: CR = achar(13)

    src = 'a'//CR//NL//'b'//CR//'c'//NL//"'x&"//NL//"&y'"//NL//'d'
    call tokenize(src, tokens)
    call check(error, tokens%n, 9)
    if (allocated(error)) return
    call check(error, token_text(src, tokens%tok(2)), CR//NL)
    if (allocated(error)) return
    call check(error, tokens%tok(3)%line == 2 .and. tokens%tok(5)%line == 3 .and. tokens%tok(7)%line == 4 &
               .and. tokens%tok(9)%line == 6, 'line numbers')
end subroutine test_line_endings

subroutine test_round_trip(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), parameter :: cases(*) = [character(len=24) :: &
                                               "'unterminated", '"a&', '&', '#', '${ x', '.', '1.', '1e', "x'", 'a b ', &
                                               '!', '&'//NL//'&', 'x='//achar(9)//'1', achar(0)//achar(200)]
    type(token_list_t) :: tokens
    integer :: k
    do k = 1, size(cases)
        call tokenize(trim(cases(k)), tokens)
        call check(error, detokenize(trim(cases(k)), tokens) == trim(cases(k)), 'round trip of "'//trim(cases(k))//'"')
        if (allocated(error)) return
    end do
end subroutine test_round_trip

end module test_lexer
