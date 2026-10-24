module ffmt_options
!! Formatting options.
use ffmt_whitespace, only: whitespace_options_t, WS_KEEP, WS_ADD, WS_REMOVE
use ffmt_case, only: case_options_t, CASE_KEEP, CASE_LOWER, CASE_UPPER
use ffmt_relational, only: REL_KEEP, REL_SYMBOLS, REL_LETTERS
implicit none
private

public :: whitespace_options_t, WS_KEEP, WS_ADD, WS_REMOVE
public :: case_options_t, CASE_KEEP, CASE_LOWER, CASE_UPPER
public :: REL_KEEP, REL_SYMBOLS, REL_LETTERS

integer, parameter, public :: CONT_ALIGN = 1    !! align continuation lines with the first item after the
                                                !! innermost open bracket, or after the first `=`
integer, parameter, public :: CONT_INDENT = 2   !! indent continuation lines by a fixed amount

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
    type(whitespace_options_t) :: spaces    !! whitespace rules within lines
    type(case_options_t) :: letter_case     !! letter case of keywords and operators
end type format_options_t

end module ffmt_options
