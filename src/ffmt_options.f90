module ffmt_options
!! Formatting options: all the settings, with their defaults, and the modes they take. The modules
!! that apply them ([[ffmt_whitespace]], [[ffmt_case]], [[ffmt_relational]], [[ffmt_layout]]) use
!! this module, not the other way round, so it depends on nothing.
implicit none
private

! layout of continuation lines (see [[ffmt_layout]])
integer, parameter, public :: CONT_ALIGN = 1    !! align continuation lines with the first item after the
                                                !! innermost open bracket, or after the first `=`
integer, parameter, public :: CONT_INDENT = 2   !! indent continuation lines by a fixed amount

! whitespace rule modes (see [[ffmt_whitespace]])
integer, parameter, public :: WS_KEEP = 0     !! leave the blanks as they are
integer, parameter, public :: WS_ADD = 1      !! at least one blank on each side
integer, parameter, public :: WS_REMOVE = 2   !! no blanks

! letter case modes (see [[ffmt_case]])
integer, parameter, public :: CASE_KEEP = 0     !! leave the case as it is
integer, parameter, public :: CASE_LOWER = 1    !! lowercase
integer, parameter, public :: CASE_UPPER = 2    !! uppercase

! spellings of relational operators (see [[ffmt_relational]])
integer, parameter, public :: REL_KEEP = 0      !! leave the spelling as it is
integer, parameter, public :: REL_SYMBOLS = 1   !! `.lt.` becomes `<`, ...
integer, parameter, public :: REL_LETTERS = 2   !! `<` becomes `.lt.`, ...

type, public :: whitespace_options_t
    !! Modes of the whitespace rules. The defaults follow the most common style in practice where it is
    !! clear-cut, and keep the rest.
    integer :: assignment = WS_ADD     !! `=` outside brackets: assignments, initializations, `do i = 1, n`
    integer :: keyword_args = WS_REMOVE !! `=` inside brackets: `f(x=1)`, `intent=`, `len=*`
    integer :: pointer = WS_ADD        !! `=>`: pointer assignment, renames, `associate`
    integer :: declarations = WS_ADD   !! `::`
    integer :: relational = WS_KEEP    !! `== /= < <= > >=` and `.eq.` etc.
    integer :: logical = WS_KEEP       !! `.and. .or. .eqv. .neqv.`
    integer :: plus_minus = WS_KEEP    !! binary `+ -`
    integer :: mult_div = WS_KEEP      !! binary `* /`
    integer :: power = WS_REMOVE       !! `**`
    integer :: concat = WS_REMOVE      !! `//`
    integer :: comma = WS_KEEP         !! `,`: with `WS_ADD`, no blank before and at least one after
    integer :: slice_colon = WS_REMOVE !! `:` inside brackets: `a(1:n)`
    integer :: percent = WS_REMOVE     !! `%`
    integer :: control_paren = WS_ADD  !! between `if`, `while`, `select case`, `where`, ... and `(`, and
                                       !! between `)` and a final `then`
end type whitespace_options_t

type, public :: case_options_t
    !! Letter case of keywords and of dot operators and logical constants.
    integer :: keywords = CASE_KEEP    !! `if`, `end do`, `integer`, `intent(in)`, ...
    integer :: operators = CASE_KEEP   !! `.and.`, `.eq.`, `.true.`, ...
end type case_options_t

type, public :: format_options_t
    !! All options that control formatting. The defaults give 4-space indentation with the bodies
    !! of modules and programs not indented, and aligned continuation lines.
    integer :: indent = 4                   !! spaces per indentation level
    integer :: base_indent = 0              !! columns added to the indentation of every line, so that even
                                            !! program units are indented (4 for code that starts in column 5)
    logical :: indent_units = .false.       !! indent the bodies of modules, submodules and main programs
    logical :: indent_procedures = .true.   !! indent the bodies of subroutines and functions
    integer :: continuation = CONT_ALIGN    !! layout of continuation lines (`CONT_ALIGN` or `CONT_INDENT`)
    integer :: continuation_indent = 4      !! extra indentation of continuation lines, used with `CONT_INDENT`
                                            !! and when there is nothing to align with
    logical :: indent_comments = .true.     !! re-indent comment lines like the code around them
    logical :: keep_column1_comments = .false. !! leave comment lines that start in column 1 there (with
                                            !! `indent_comments`)
    integer :: line_length = 132            !! maximum line length (0 for no limit). Alignment that would make a
                                            !! line longer falls back to a smaller indentation.
    logical :: split_lines = .true.         !! split code lines that are longer than `line_length`
    integer :: relational = REL_KEEP        !! spelling of relational operators: `REL_KEEP`, `REL_SYMBOLS`
                                            !! (`.lt.` becomes `<`) or `REL_LETTERS` (`<` becomes `.lt.`)
    logical :: sort_use = .false.           !! sort the `use` statements of each group by module name
    logical :: sort_use_only = .false.      !! sort the names in `only:` lists
    type(whitespace_options_t) :: spaces    !! whitespace rules within lines
    type(case_options_t) :: letter_case     !! letter case of keywords and operators
end type format_options_t

end module ffmt_options
