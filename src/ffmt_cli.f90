module ffmt_cli
!! Command-line interface: argument parsing and processing of files and directories.
use iso_fortran_env, only: output_unit, error_unit
use ffmt_options
use ffmt_settings, only: setting_t, apply_setting
use ffmt_config, only: config_t, find_config, load_config
use ffmt_format, only: format_text, diagnostic_t, FMT_OK
use ffmt_io, only: read_file, write_file, read_stdin
use ffmt_fs, only: is_directory, absolute_path, parent_directory
use ffmt_walk, only: collect_files, default_extensions, default_exclude
use ffmt_diff, only: unified_diff
use ffmt_strings, only: string_t, to_s
implicit none
private

public :: parse_args, run, usage, string_t

! exit codes
integer, parameter, public :: EXIT_OK = 0         !! success
integer, parameter, public :: EXIT_CHANGED = 1    !! with `--check`: some files are not formatted
integer, parameter, public :: EXIT_ERROR = 2      !! bad arguments, unreadable files or internal errors

character(len=*), parameter, public :: VERSION = '0.1.0'

type, public :: cli_t
    !! Parsed command line.
    type(setting_t), allocatable :: settings(:)     !! formatting settings given on the command line, in order
    type(string_t), allocatable :: paths(:)         !! files, directories, or `-` for standard input
    type(string_t), allocatable :: exclude(:)       !! `--exclude` patterns, added to the configured ones
    type(string_t), allocatable :: extensions(:)    !! `--extensions`; not allocated if not given
    character(len=:), allocatable :: config         !! `--config` file, or `''`
    logical :: no_config = .false.                  !! ignore configuration files
    logical :: stdout = .false.                     !! write results to standard output
    logical :: check = .false.                      !! report files that would change; write nothing
    logical :: diff = .false.                       !! write a diff of the changes; write nothing else
    logical :: quiet = .false.                      !! do not report warnings
    logical :: help = .false.
    logical :: version = .false.
end type cli_t

type :: config_cache_t
    !! Configurations already found, by directory.
    type(string_t), allocatable :: dirs(:)
    type(config_t), allocatable :: configs(:)
end type config_cache_t

contains

function usage() result(text)
    !! The `--help` text.
    character(len=:), allocatable :: text
    character, parameter :: nl = new_line('a')
    text = 'Usage: ffmt [OPTIONS] PATH...'//nl// &
           nl// &
           'Format free-form Fortran source files in place. A directory is searched recursively for'//nl// &
           'Fortran files; "-" reads standard input and writes standard output.'//nl// &
           nl// &
           'Formatting:'//nl// &
           '  -i, --indent N              spaces per indentation level (default 4)'//nl// &
           '      --base-indent N         columns added to every line, program units included (default 0)'//nl// &
           '      --indent-units          indent the bodies of modules, submodules and programs'//nl// &
           '      --no-indent-procedures  do not indent the bodies of subroutines and functions'//nl// &
           '      --continuation STYLE    continuation lines: "align" with brackets and "=" (default),'//nl// &
           '                              or "indent" by a fixed amount'//nl// &
           '      --continuation-indent N extra indentation of continuation lines (default 4)'//nl// &
           '      --no-indent-comments    leave the indentation of comment lines unchanged'//nl// &
           '      --keep-column1-comments leave comment lines that start in column 1 there'//nl// &
           '  -l, --line-length N         maximum line length (default 132, 0 for no limit): longer code'//nl// &
           '                              lines are split, and alignment uses less indentation'//nl// &
           '                              rather than exceed it'//nl// &
           '      --no-split              do not split long lines'//nl// &
           '      --sort-use              sort each group of use statements by module name'//nl// &
           '      --sort-use-only         sort the names in only: lists'//nl// &
           '      --relational STYLE      spelling of relational operators: "symbols" (.lt. becomes <),'//nl// &
           '                              "letters" (< becomes .lt.) or "keep" (default)'//nl// &
           '      --spaces RULE=MODE,...  blanks around operators; MODE is add (at least one blank),'//nl// &
           '                              remove or keep. RULE is one of assignment, keyword-args,'//nl// &
           '                              pointer, declarations, relational, logical, plus-minus,'//nl// &
           '                              mult-div, power, concat, comma, slice-colon, percent,'//nl// &
           '                              control-paren, or all'//nl// &
           '      --case WHAT=MODE,...    letter case; WHAT is keywords, operators or all, MODE is'//nl// &
           '                              lower, upper or keep (default keep)'//nl// &
           '      --set KEY=VALUE         any setting, as in a configuration file (e.g. spaces.comma=add)'//nl// &
           nl// &
           'Files:'//nl// &
           '      --config FILE           read settings from FILE instead of searching for one'//nl// &
           '      --no-config             do not read configuration files'//nl// &
           '      --exclude PATTERN       skip matching files and directories in searched directories'//nl// &
           '                              (repeatable; hidden entries and "build" are skipped by default)'//nl// &
           '      --extensions LIST       extensions of the files to format in searched directories'//nl// &
           '                              (default f90,F90,f95,F95,f03,F03,f08,F08,f18,F18)'//nl// &
           nl// &
           'Output:'//nl// &
           '  -s, --stdout                write the results to standard output'//nl// &
           '  -d, --diff                  write the changes as a unified diff; change no files'//nl// &
           '  -c, --check                 change no files; exit with status 1 if a file would change'//nl// &
           '  -q, --quiet                 do not report warnings'//nl// &
           '  -h, --help                  show this help'//nl// &
           '      --version               show the version'//nl// &
           nl// &
           'Settings come from the defaults, then the nearest .ffmt.toml (or an [extra.ffmt] table in'//nl// &
           'fpm.toml) in or above the file''s directory, then the command line.'//nl// &
           nl// &
           'Formatter directives: a statement with a "!&" comment is left unchanged, and lines'//nl// &
           'between "!&<" and "!&>" are left unchanged.'//nl// &
           nl// &
           'Exit status: 0 on success, 1 if --check found files to format, 2 on errors.'
end function usage

subroutine parse_args(args, cli, stat, errmsg)
    !! Parse command-line arguments (without the program name).
    type(string_t), intent(in) :: args(:)
    type(cli_t), intent(out) :: cli
    integer, intent(out) :: stat
    character(len=:), allocatable, intent(out) :: errmsg
    type(format_options_t) :: scratch
    integer :: k, j
    character(len=:), allocatable :: arg
    logical :: only_paths

    stat = 0
    allocate (cli%settings(0), cli%paths(0), cli%exclude(0))
    cli%config = ''
    only_paths = .false.
    k = 0
    do while (k < size(args))
        k = k + 1
        arg = args(k)%s
        if (only_paths .or. len(arg) == 0 .or. arg == '-') then
            cli%paths = [cli%paths, string_t(arg)]
            cycle
        end if
        if (arg(1:1) /= '-') then
            cli%paths = [cli%paths, string_t(arg)]
            cycle
        end if
        select case (arg)
        case ('--')
            only_paths = .true.
        case ('-h', '--help')
            cli%help = .true.
        case ('--version')
            cli%version = .true.
        case ('-s', '--stdout')
            cli%stdout = .true.
        case ('-c', '--check')
            cli%check = .true.
        case ('-d', '--diff')
            cli%diff = .true.
        case ('-q', '--quiet')
            cli%quiet = .true.
        case ('--no-config')
            cli%no_config = .true.
        case ('--indent-units')
            call add('indent-units', 'true')
        case ('--no-indent-procedures')
            call add('indent-procedures', 'false')
        case ('--no-indent-comments')
            call add('indent-comments', 'false')
        case ('--keep-column1-comments')
            call add('column1-comments', 'keep')
        case ('--no-split')
            call add('split', 'false')
        case ('--sort-use')
            call add('sort-use', 'true')
        case ('--sort-use-only')
            call add('sort-use-only', 'true')
        case default
            if (.not. takes_value(arg)) then
                call fail('unknown option '//arg)
                return
            end if
            if (k == size(args)) then
                call fail('missing value for '//arg)
                return
            end if
            k = k + 1
            call option_value(arg, args(k)%s)
            if (stat /= 0) return
        end select
    end do

    ! check the settings now, so that a mistake is reported before any file is changed
    do j = 1, size(cli%settings)
        call apply_setting(scratch, cli%settings(j)%key, cli%settings(j)%value, stat, errmsg)
        if (stat /= 0) then
            stat = EXIT_ERROR
            return
        end if
    end do
    if (.not. (cli%help .or. cli%version) .and. size(cli%paths) == 0) call fail('no files given')

contains

    logical function takes_value(opt)
        character(len=*), intent(in) :: opt
        select case (opt)
        case ('-i', '--indent', '--base-indent', '--continuation', '--continuation-indent', '-l', '--line-length', '--relational', &
              '--spaces', '--case', '--set', '--config', '--exclude', '--extensions')
            takes_value = .true.
        case default
            takes_value = .false.
        end select
    end function takes_value

    subroutine option_value(opt, value)
        character(len=*), intent(in) :: opt, value
        select case (opt)
        case ('-i', '--indent')
            call add('indent', value)
        case ('--base-indent')
            call add('base-indent', value)
        case ('--continuation')
            call add('continuation', value)
        case ('--continuation-indent')
            call add('continuation-indent', value)
        case ('-l', '--line-length')
            call add('line-length', value)
        case ('--relational')
            call add('relational', value)
        case ('--spaces')
            call add_list('spaces.', value)
        case ('--case')
            call add_list('case.', value)
        case ('--set')
            call add_list('', value)
        case ('--config')
            cli%config = value
        case ('--exclude')
            cli%exclude = [cli%exclude, string_t(value)]
        case ('--extensions')
            call extension_list(value)
        end select
    end subroutine option_value

    subroutine add(key, value)
        character(len=*), intent(in) :: key, value
        cli%settings = [cli%settings, setting_t(key, value)]
    end subroutine add

    subroutine add_list(prefix, text)
        !! Comma-separated `NAME=VALUE` items.
        character(len=*), intent(in) :: prefix, text
        integer :: i, j, eq
        character(len=:), allocatable :: item
        i = 1
        do while (i <= len(text))
            j = index(text(i:), ',')
            if (j == 0) then
                j = len(text) + 1
            else
                j = i + j - 1
            end if
            item = text(i:j - 1)
            eq = index(item, '=')
            if (eq == 0) then
                call fail('expected NAME=VALUE for '//arg//', not "'//item//'"')
                return
            end if
            call add(prefix//item(1:eq - 1), item(eq + 1:))
            i = j + 1
        end do
    end subroutine add_list

    subroutine extension_list(text)
        character(len=*), intent(in) :: text
        integer :: i, j
        allocate (cli%extensions(0))
        i = 1
        do while (i <= len(text))
            j = index(text(i:), ',')
            if (j == 0) then
                j = len(text) + 1
            else
                j = i + j - 1
            end if
            if (j > i) then
                if (text(i:i) == '.') then
                    cli%extensions = [cli%extensions, string_t(text(i + 1:j - 1))]
                else
                    cli%extensions = [cli%extensions, string_t(text(i:j - 1))]
                end if
            end if
            i = j + 1
        end do
    end subroutine extension_list

    subroutine fail(msg)
        character(len=*), intent(in) :: msg
        stat = EXIT_ERROR
        errmsg = msg
    end subroutine fail

end subroutine parse_args

integer function run(cli) result(status)
    !! Format the paths of `cli` and return the exit status.
    type(cli_t), intent(in) :: cli
    type(config_cache_t) :: cache
    type(string_t), allocatable :: files(:)
    integer :: k, j

    if (cli%help) then
        write (output_unit, '(a)') usage()
        status = EXIT_OK
        return
    end if
    if (cli%version) then
        write (output_unit, '(a)') 'ffmt '//VERSION
        status = EXIT_OK
        return
    end if
    allocate (cache%dirs(0), cache%configs(0))
    status = EXIT_OK
    do k = 1, size(cli%paths)
        associate (path => cli%paths(k)%s)
            if (path == '-') then
                status = max(status, process_stdin(cli, cache))
            else if (is_directory(path)) then
                call directory_files(cli, cache, path, files, status)
                do j = 1, size(files)
                    status = max(status, process_file(cli, cache, files(j)%s))
                end do
            else
                status = max(status, process_file(cli, cache, path))
            end if
        end associate
    end do
end function run

subroutine directory_files(cli, cache, dir, files, status)
    !! The Fortran files under `dir`, selected with the `[files]` settings of its configuration and
    !! the command line.
    type(cli_t), intent(in) :: cli
    type(config_cache_t), intent(inout) :: cache
    character(len=*), intent(in) :: dir
    type(string_t), allocatable, intent(out) :: files(:)
    integer, intent(inout) :: status
    type(config_t) :: cfg
    type(string_t), allocatable :: extensions(:), exclude(:)
    integer :: stat
    character(len=:), allocatable :: errmsg

    extensions = default_extensions()
    exclude = default_exclude()
    call config_for(cli, cache, absolute_path(dir), cfg, stat, errmsg)
    if (stat /= 0) then
        write (error_unit, '(a)') 'error: '//errmsg
        status = EXIT_ERROR
        allocate (files(0))
        return
    end if
    if (allocated(cfg%extensions)) extensions = cfg%extensions
    if (allocated(cfg%exclude)) exclude = cfg%exclude
    if (allocated(cli%extensions)) extensions = cli%extensions
    exclude = [exclude, cli%exclude]
    call collect_files(dir, extensions, exclude, files)
end subroutine directory_files

subroutine config_for(cli, cache, dir, cfg, stat, errmsg)
    !! The configuration that applies in directory `dir` (absolute).
    type(cli_t), intent(in) :: cli
    type(config_cache_t), intent(inout) :: cache
    character(len=*), intent(in) :: dir
    type(config_t), intent(out) :: cfg
    integer, intent(out) :: stat
    character(len=:), allocatable, intent(out) :: errmsg
    logical :: found
    integer :: k

    stat = 0
    if (cli%no_config) then
        cfg%path = ''
        allocate (cfg%settings(0))
        return
    end if
    if (len(cli%config) > 0) then
        call load_config(cli%config, ends_with(cli%config, 'fpm.toml'), cfg, found, stat, errmsg)
        if (stat == 0 .and. .not. found) then
            stat = 1
            errmsg = cli%config//': no [extra.ffmt] table'
        end if
        return
    end if
    do k = 1, size(cache%dirs)
        if (cache%dirs(k)%s == dir .and. len(cache%dirs(k)%s) == len(dir)) then
            cfg = cache%configs(k)
            return
        end if
    end do
    call find_config(dir, cfg, stat, errmsg)
    if (stat /= 0) return
    cache%dirs = [cache%dirs, string_t(dir)]
    cache%configs = [cache%configs, cfg]
end subroutine config_for

subroutine options_for(cli, cache, dir, opts, stat, errmsg)
    !! The options for a file in directory `dir`: defaults, then configuration, then command line.
    type(cli_t), intent(in) :: cli
    type(config_cache_t), intent(inout) :: cache
    character(len=*), intent(in) :: dir
    type(format_options_t), intent(out) :: opts
    integer, intent(out) :: stat
    character(len=:), allocatable, intent(out) :: errmsg
    type(config_t) :: cfg
    integer :: k

    call config_for(cli, cache, absolute_path(dir), cfg, stat, errmsg)
    if (stat /= 0) return
    do k = 1, size(cfg%settings)
        call apply_setting(opts, cfg%settings(k)%key, cfg%settings(k)%value, stat, errmsg)
        if (stat /= 0) then
            errmsg = cfg%path//': '//errmsg
            return
        end if
    end do
    do k = 1, size(cli%settings)
        call apply_setting(opts, cli%settings(k)%key, cli%settings(k)%value, stat, errmsg)
        if (stat /= 0) return
    end do
end subroutine options_for

integer function process_stdin(cli, cache) result(status)
    !! Format standard input to standard output (or report it with `--check` / `--diff`).
    type(cli_t), intent(in) :: cli
    type(config_cache_t), intent(inout) :: cache
    character(len=:), allocatable :: text
    call read_stdin(text)
    status = process_text(cli, cache, '<stdin>', '.', text, .true.)
end function process_stdin

integer function process_file(cli, cache, path) result(status)
    type(cli_t), intent(in) :: cli
    type(config_cache_t), intent(inout) :: cache
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: text, errmsg, dir
    integer :: stat

    call read_file(path, text, stat, errmsg)
    if (stat /= 0) then
        write (error_unit, '(a)') path//': error: '//errmsg
        status = EXIT_ERROR
        return
    end if
    dir = parent_directory(absolute_path(path))
    status = process_text(cli, cache, path, dir, text, cli%stdout)
end function process_file

integer function process_text(cli, cache, name, dir, text, to_stdout) result(status)
    !! Format `text` from file `name` in directory `dir`, and write or report the result.
    type(cli_t), intent(in) :: cli
    type(config_cache_t), intent(inout) :: cache
    character(len=*), intent(in) :: name, dir, text
    logical, intent(in) :: to_stdout
    type(format_options_t) :: opts
    character(len=:), allocatable :: out, errmsg
    type(diagnostic_t), allocatable :: diags(:)
    integer :: stat, d
    logical :: changed

    status = EXIT_OK
    call options_for(cli, cache, dir, opts, stat, errmsg)
    if (stat /= 0) then
        write (error_unit, '(a)') name//': error: '//errmsg
        status = EXIT_ERROR
        return
    end if
    call format_text(text, opts, out, stat, errmsg, diags)
    if (.not. cli%quiet) then
        do d = 1, size(diags)
            write (error_unit, '(a)') name//':'//to_s(diags(d)%line)//': warning: '//diags(d)%message
        end do
    end if
    if (stat /= FMT_OK) then
        write (error_unit, '(a)') name//': error: '//errmsg
        status = EXIT_ERROR
        return
    end if

    changed = out /= text .or. len(out) /= len(text)
    if (cli%diff .and. changed) write (output_unit, '(a)', advance='no') unified_diff(text, out, name)
    if (cli%check) then
        if (changed) then
            if (.not. cli%diff) write (output_unit, '(a)') name//': would be reformatted'
            status = EXIT_CHANGED
        end if
    else if (cli%diff) then
        continue
    else if (to_stdout) then
        write (output_unit, '(a)', advance='no') out
    else if (changed) then
        call write_file(name, out, stat, errmsg)
        if (stat /= 0) then
            write (error_unit, '(a)') name//': error: '//errmsg
            status = EXIT_ERROR
        end if
    end if
end function process_text

pure logical function ends_with(s, suffix)
    character(len=*), intent(in) :: s, suffix
    ends_with = .false.
    if (len(s) >= len(suffix)) ends_with = s(len(s) - len(suffix) + 1:) == suffix
end function ends_with

end module ffmt_cli
