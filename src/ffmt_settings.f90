module ffmt_settings
!! Named settings, as written in a config file or given on the command line, applied to the options.
!!
!! | Setting | Values |
!! |---|---|
!! | `indent`, `base-indent`, `continuation-indent` | 0 to 64 |
!! | `indent-units`, `indent-procedures`, `indent-comments`, `split`, `sort-use`, `sort-use-only` | `true`, `false` |
!! | `continuation` | `align`, `indent` |
!! | `column1-comments` | `indent` (like the code around them), `keep` (stay in column 1) |
!! | `line-length` | 0 (no limit) to 100000 |
!! | `relational` | `symbols`, `letters`, `keep` |
!! | `spaces.RULE` (or `spaces.all`) | `add`, `remove`, `keep` |
!! | `case.keywords`, `case.operators` (or `case.all`) | `lower`, `upper`, `keep` |
use ffmt_options
use ffmt_strings, only: to_s
implicit none
private

public :: apply_setting

type, public :: setting_t
    !! A named setting with its value as text.
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
end type setting_t

contains

subroutine apply_setting(opts, key, value, stat, errmsg)
    !! Apply one setting to `opts`. `stat` is nonzero, with a message, for an unknown setting or a bad value.
    type(format_options_t), intent(inout) :: opts
    character(len=*), intent(in) :: key, value
    integer, intent(out) :: stat
    character(len=:), allocatable, intent(out) :: errmsg

    stat = 0
    select case (key)
    case ('indent')
        call read_count(64, opts%indent)
    case ('base-indent')
        call read_count(64, opts%base_indent)
    case ('continuation-indent')
        call read_count(64, opts%continuation_indent)
    case ('line-length')
        call read_count(100000, opts%line_length)
    case ('indent-units')
        call read_flag(opts%indent_units)
    case ('indent-procedures')
        call read_flag(opts%indent_procedures)
    case ('indent-comments')
        call read_flag(opts%indent_comments)
    case ('column1-comments')
        select case (value)
        case ('keep')
            opts%keep_column1_comments = .true.
        case ('indent')
            opts%keep_column1_comments = .false.
        case default
            call fail('"keep" or "indent"')
        end select
    case ('split')
        call read_flag(opts%split_lines)
    case ('sort-use')
        call read_flag(opts%sort_use)
    case ('sort-use-only')
        call read_flag(opts%sort_use_only)
    case ('continuation')
        select case (value)
        case ('align')
            opts%continuation = CONT_ALIGN
        case ('indent')
            opts%continuation = CONT_INDENT
        case default
            call fail('"align" or "indent"')
        end select
    case ('relational')
        select case (value)
        case ('symbols')
            opts%relational = REL_SYMBOLS
        case ('letters')
            opts%relational = REL_LETTERS
        case ('keep')
            opts%relational = REL_KEEP
        case default
            call fail('"symbols", "letters" or "keep"')
        end select
    case default
        if (index(key, 'spaces.') == 1) then
            call set_spaces(key(8:))
        else if (index(key, 'case.') == 1) then
            call set_case(key(6:))
        else
            stat = 1
            errmsg = 'unknown setting "'//key//'"'
        end if
    end select

contains

    subroutine fail(expected)
        character(len=*), intent(in) :: expected
        stat = 1
        errmsg = 'expected '//expected//' for "'//key//'", not "'//value//'"'
    end subroutine fail

    subroutine read_count(max_value, n)
        integer, intent(in) :: max_value
        integer, intent(inout) :: n
        integer :: ios, v
        if (verify(value, '0123456789') == 0 .and. len(value) > 0 .and. len(value) <= 9) then
            read (value, *, iostat=ios) v
            if (ios == 0 .and. v <= max_value) then
                n = v
                return
            end if
        end if
        call fail('a number from 0 to '//to_s(max_value))
    end subroutine read_count

    subroutine read_flag(flag)
        logical, intent(inout) :: flag
        select case (value)
        case ('true')
            flag = .true.
        case ('false')
            flag = .false.
        case default
            call fail('true or false')
        end select
    end subroutine read_flag

    subroutine set_spaces(rule)
        character(len=*), intent(in) :: rule
        integer :: mode
        select case (value)
        case ('add')
            mode = WS_ADD
        case ('remove')
            mode = WS_REMOVE
        case ('keep')
            mode = WS_KEEP
        case default
            call fail('add, remove or keep')
            return
        end select
        associate (ws => opts%spaces)
            select case (rule)
            case ('all')
                ws = whitespace_options_t(mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, &
                                          mode, mode)
            case ('assignment')
                ws%assignment = mode
            case ('keyword-args')
                ws%keyword_args = mode
            case ('pointer')
                ws%pointer = mode
            case ('declarations')
                ws%declarations = mode
            case ('relational')
                ws%relational = mode
            case ('logical')
                ws%logical = mode
            case ('plus-minus')
                ws%plus_minus = mode
            case ('mult-div')
                ws%mult_div = mode
            case ('power')
                ws%power = mode
            case ('concat')
                ws%concat = mode
            case ('comma')
                ws%comma = mode
            case ('slice-colon')
                ws%slice_colon = mode
            case ('percent')
                ws%percent = mode
            case ('control-paren')
                ws%control_paren = mode
            case default
                stat = 1
                errmsg = 'unknown whitespace rule "'//rule//'"'
            end select
        end associate
    end subroutine set_spaces

    subroutine set_case(what)
        character(len=*), intent(in) :: what
        integer :: mode
        select case (value)
        case ('lower')
            mode = CASE_LOWER
        case ('upper')
            mode = CASE_UPPER
        case ('keep')
            mode = CASE_KEEP
        case default
            call fail('lower, upper or keep')
            return
        end select
        select case (what)
        case ('all')
            opts%letter_case = case_options_t(mode, mode)
        case ('keywords')
            opts%letter_case%keywords = mode
        case ('operators')
            opts%letter_case%operators = mode
        case default
            stat = 1
            errmsg = 'unknown case setting "'//what//'"'
        end select
    end subroutine set_case

end subroutine apply_setting

end module ffmt_settings
