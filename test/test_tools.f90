module test_tools
!! Tests of the file system, directory walk, configuration, settings, diff and command-line modules.
!! They use the fixtures in `fixtures/`, relative to the package root (where `fpm test` runs).
use testdrive, only: new_unittest, unittest_type, error_type, check
use ffmt_fs
use ffmt_walk
use ffmt_config
use ffmt_settings
use ffmt_diff
use ffmt_cli
use ffmt_options
use ffmt_strings, only: string_t
use fregex, only: regex_t, regex
implicit none
private

public :: collect_tools

character, parameter :: NL = achar(10)

contains

subroutine collect_tools(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("list directory", test_list_directory), &
        new_unittest("paths", test_paths), &
        new_unittest("glob patterns", test_globs), &
        new_unittest("collect files", test_collect), &
        new_unittest("config file", test_config), &
        new_unittest("config in fpm.toml", test_fpm_config), &
        new_unittest("settings", test_settings), &
        new_unittest("diff", test_diff), &
        new_unittest("command line", test_command_line) &
        ]
end subroutine collect_tools

integer function find(entries, name) result(k)
    type(dir_entry_t), intent(in) :: entries(:)
    character(len=*), intent(in) :: name
    do k = 1, size(entries)
        if (entries(k)%name == name) return
    end do
    k = 0
end function find

subroutine test_list_directory(error)
    type(error_type), allocatable, intent(out) :: error
    type(dir_entry_t), allocatable :: entries(:)
    integer :: stat, k

    call list_directory('fixtures/tree', entries, stat)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, size(entries), 8)
    if (allocated(error)) return
    k = find(entries, 'a.f90')
    call check(error, k > 0, 'a.f90 listed')
    if (allocated(error)) return
    call check(error, entries(k)%kind, ENTRY_FILE)
    if (allocated(error)) return
    k = find(entries, 'sub')
    call check(error, k > 0 .and. entries(max(k, 1))%kind == ENTRY_DIR, 'sub is a directory')
    if (allocated(error)) return
    call check(error, find(entries, '.') == 0 .and. find(entries, '..') == 0, 'no . or ..')
    if (allocated(error)) return
    call list_directory('fixtures/no-such-directory', entries, stat)
    call check(error, stat /= 0, 'missing directory')
    if (allocated(error)) return
    call check(error, is_directory('fixtures') .and. .not. is_directory('fixtures/tree/a.f90'))
end subroutine test_list_directory

subroutine test_paths(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, join_path('a', 'b'), 'a/b')
    if (allocated(error)) return
    call check(error, join_path('a/', 'b'), 'a/b')
    if (allocated(error)) return
    call check(error, parent_directory('/x/y/z'), '/x/y')
    if (allocated(error)) return
    call check(error, parent_directory('/x/y/'), '/x')
    if (allocated(error)) return
    call check(error, parent_directory('/x'), '/')
    if (allocated(error)) return
    call check(error, parent_directory('/'), '')
    if (allocated(error)) return
    call check(error, parent_directory('C:\a\b'), 'C:\a')
    if (allocated(error)) return
    call check(error, parent_directory('C:\a'), 'C:\')
    if (allocated(error)) return
    call check(error, absolute_path('/abs/path'), '/abs/path')
    if (allocated(error)) return
    call check(error, absolute_path('rel') == join_path(current_directory(), 'rel'))
end subroutine test_paths

logical function glob_matches(glob, path)
    character(len=*), intent(in) :: glob, path
    type(regex_t) :: re
    re = regex(glob_to_regex(glob))
    glob_matches = re%fullmatch(path)
end function glob_matches

subroutine test_globs(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, glob_matches('*.f90', 'a.f90') .and. .not. glob_matches('*.f90', 'a.F90'))
    if (allocated(error)) return
    call check(error, .not. glob_matches('*.f90', 'sub/a.f90'), '* stays in a component')
    if (allocated(error)) return
    call check(error, glob_matches('src/**/*.f90', 'src/a/b/c.f90') .and. glob_matches('src/**/*.f90', 'src/c.f90'))
    if (allocated(error)) return
    call check(error, glob_matches('?.f9[0-5]', 'x.f95') .and. .not. glob_matches('?.f9[!0-5]', 'x.f95'))
    if (allocated(error)) return
    call check(error, glob_matches('a+b.(f)', 'a+b.(f)'), 'regex characters are literal')
end subroutine test_globs

subroutine test_collect(error)
    type(error_type), allocatable, intent(out) :: error
    type(string_t), allocatable :: files(:)

    call collect_files('fixtures/tree', default_extensions(), default_exclude(), files)
    call check(error, size(files), 4)
    if (allocated(error)) return
    call check(error, files(1)%s == 'fixtures/tree/a.f90' .and. files(2)%s == 'fixtures/tree/b.F90' .and. &
               files(3)%s == 'fixtures/tree/skip_me.f90' .and. files(4)%s == 'fixtures/tree/sub/c.f90', &
               'default selection')
    if (allocated(error)) return
    call collect_files('fixtures/tree', [string_t('f90')], [string_t('skip*'), string_t('sub/*')], files)
    call check(error, size(files) == 3, 'extensions and path patterns')
end subroutine test_collect

subroutine test_config(error)
    type(error_type), allocatable, intent(out) :: error
    type(config_t) :: cfg
    type(format_options_t) :: opts
    integer :: stat, k
    character(len=:), allocatable :: errmsg

    call find_config(absolute_path('fixtures/tree'), cfg, stat, errmsg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, index(cfg%path, 'fixtures/tree/.fformat.toml') > 0, 'found '//cfg%path)
    if (allocated(error)) return
    do k = 1, size(cfg%settings)
        call apply_setting(opts, cfg%settings(k)%key, cfg%settings(k)%value, stat, errmsg)
        call check(error, stat, 0)
        if (allocated(error)) return
    end do
    call check(error, opts%indent == 2 .and. opts%line_length == 100, 'indent and line length')
    if (allocated(error)) return
    ! `all` is applied first, so `power` overrides it
    call check(error, opts%spaces%comma == WS_ADD .and. opts%spaces%power == WS_REMOVE, 'spaces')
    if (allocated(error)) return
    call check(error, opts%letter_case%keywords, CASE_LOWER)
    if (allocated(error)) return
    call check(error, allocated(cfg%exclude) .and. .not. allocated(cfg%extensions), 'files table')
    if (allocated(error)) return
    call check(error, size(cfg%exclude) == 3 .and. cfg%exclude(1)%s == 'skip*', 'exclude list')
    if (allocated(error)) return
    ! the nearest file wins, also for subdirectories without one
    call find_config(absolute_path('fixtures/tree/sub'), cfg, stat, errmsg)
    call check(error, size(cfg%settings) == 1 .and. cfg%settings(1)%value == '6', 'sub configuration')
end subroutine test_config

subroutine test_fpm_config(error)
    type(error_type), allocatable, intent(out) :: error
    type(config_t) :: cfg
    type(format_options_t) :: opts
    integer :: stat, k
    character(len=:), allocatable :: errmsg

    call find_config(absolute_path('fixtures/fpm/src'), cfg, stat, errmsg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, index(cfg%path, 'fpm.toml') > 0, 'found '//cfg%path)
    if (allocated(error)) return
    do k = 1, size(cfg%settings)
        call apply_setting(opts, cfg%settings(k)%key, cfg%settings(k)%value, stat, errmsg)
    end do
    call check(error, opts%indent == 3 .and. opts%relational == REL_SYMBOLS .and. &
               opts%letter_case%keywords == CASE_UPPER .and. opts%letter_case%operators == CASE_UPPER)
end subroutine test_fpm_config

subroutine test_settings(error)
    type(error_type), allocatable, intent(out) :: error
    type(format_options_t) :: opts
    integer :: stat
    character(len=:), allocatable :: errmsg

    call apply_setting(opts, 'spaces.comma', 'add', stat, errmsg)
    call check(error, stat == 0 .and. opts%spaces%comma == WS_ADD)
    if (allocated(error)) return
    call apply_setting(opts, 'indent', '200', stat, errmsg)
    call check(error, errmsg, 'expected a number from 0 to 64 for "indent", not "200"')
    if (allocated(error)) return
    call apply_setting(opts, 'colour', 'blue', stat, errmsg)
    call check(error, errmsg, 'unknown setting "colour"')
    if (allocated(error)) return
    call apply_setting(opts, 'split', 'maybe', stat, errmsg)
    call check(error, stat /= 0, 'bad flag')
end subroutine test_settings

subroutine test_diff(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: a, b
    integer :: k

    call check(error, unified_diff('same'//NL, 'same'//NL, 'f'), '')
    if (allocated(error)) return
    call check(error, unified_diff('a'//NL//'b'//NL//'c'//NL, 'a'//NL//'B'//NL//'c'//NL, 'f'), &
               '--- f'//NL//'+++ f'//NL//'@@ -1,3 +1,3 @@'//NL//' a'//NL//'-b'//NL//'+B'//NL//' c'//NL)
    if (allocated(error)) return
    call check(error, unified_diff('x', 'y', 'f'), '--- f'//NL//'+++ f'//NL//'@@ -1 +1 @@'//NL// &
               '-x'//NL//'\ No newline at end of file'//NL//'+y'//NL//'\ No newline at end of file'//NL)
    if (allocated(error)) return
    ! two changes far apart are two hunks, with three lines of context each
    a = ''
    do k = 1, 20
        a = a//repeat('x', k)//NL
    end do
    b = 'changed'//a(2:len(a) - 3)//'z'//NL
    call check(error, count_hunks(unified_diff(a, b, 'f')), 2)
end subroutine test_diff

integer function count_hunks(diff) result(n)
    character(len=*), intent(in) :: diff
    integer :: k
    n = 0
    do k = 1, len(diff) - 2
        if (diff(k:k + 2) == '@@ ') then
            if (k == 1) then
                n = n + 1
            else if (diff(k - 1:k - 1) == NL) then
                n = n + 1
            end if
        end if
    end do
end function count_hunks

subroutine test_command_line(error)
    type(error_type), allocatable, intent(out) :: error
    type(cli_t) :: cli
    integer :: stat
    character(len=:), allocatable :: errmsg

    call parse_args([string_t('-i'), string_t('3'), string_t('--spaces'), string_t('comma=add,power=keep'), &
                     string_t('--no-split'), string_t('--exclude'), string_t('gen'), string_t('-c'), &
                     string_t('src'), string_t('-')], cli, stat, errmsg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, size(cli%settings) == 4 .and. size(cli%paths) == 2 .and. cli%check, 'parsed')
    if (allocated(error)) return
    call check(error, cli%settings(2)%key == 'spaces.comma' .and. cli%settings(4)%key == 'split', 'keys')
    if (allocated(error)) return
    call parse_args([string_t('--case'), string_t('keywords=shout'), string_t('x.f90')], cli, stat, errmsg)
    call check(error, errmsg, 'expected lower, upper or keep for "case.keywords", not "shout"')
    if (allocated(error)) return
    call parse_args([string_t('--frobnicate')], cli, stat, errmsg)
    call check(error, errmsg, 'unknown option --frobnicate')
    if (allocated(error)) return
    call parse_args([string_t('-i')], cli, stat, errmsg)
    call check(error, errmsg, 'missing value for -i')
end subroutine test_command_line

end module test_tools
