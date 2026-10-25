module ffmt_config
!! Configuration files.
!!
!! Settings (see [[ffmt_settings]]) are read from the nearest of these, searching from a file's
!! directory upward:
!!
!! * `.fformat.toml`
!! * `fpm.toml`, in an `[extra.fformat]` table
!!
!! Settings are written as in the table of [[ffmt_settings]], with `spaces` and `case` as tables:
!!
!! ```toml
!! indent = 3
!! line-length = 100
!! [spaces]
!! comma = "add"
!! [case]
!! keywords = "lower"
!! [files]
!! extensions = ["f90", "F90"]
!! exclude = ["build", "legacy/*.f90"]
!! ```
!!
!! The `[files]` table controls which files are formatted when a directory is given.
use tomlf, only: toml_table, toml_array, toml_keyval, toml_key, toml_value, toml_error, toml_load, get_value, len
use tomlf_constants, only: toml_type
use ffmt_settings, only: setting_t
use ffmt_fs, only: join_path, parent_directory
use ffmt_strings, only: to_s, string_t
implicit none
private

public :: find_config, load_config

character(len=*), parameter :: CONFIG_NAME = '.fformat.toml'

type, public :: config_t
    !! The settings of one configuration file.
    character(len=:), allocatable :: path               !! the file, or `''` if there is none
    type(setting_t), allocatable :: settings(:)         !! the formatting settings, in the order they are applied
    type(string_t), allocatable :: extensions(:)      !! `files.extensions`; not allocated if the file does not set it
    type(string_t), allocatable :: exclude(:)           !! `files.exclude`, likewise
end type config_t

contains

subroutine find_config(dir, cfg, stat, errmsg)
    !! The configuration for files in directory `dir` (an absolute path): from the nearest config file
    !! in `dir` or its parents. `cfg%path` is `''` if there is none.
    character(len=*), intent(in) :: dir   !! absolute path of the directory
    type(config_t), intent(out) :: cfg   !! the configuration found
    integer, intent(out) :: stat                            !! nonzero if the config file found cannot be read
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, with the file name, if `stat` is nonzero
    character(len=:), allocatable :: d, candidate
    logical :: exists, found

    stat = 0
    d = dir
    do while (len(d) > 0)
        candidate = join_path(d, CONFIG_NAME)
        inquire (file=candidate, exist=exists)
        if (exists) then
            call load_config(candidate, .false., cfg, found, stat, errmsg)
            return
        end if
        candidate = join_path(d, 'fpm.toml')
        inquire (file=candidate, exist=exists)
        if (exists) then
            call load_config(candidate, .true., cfg, found, stat, errmsg)
            if (found .or. stat /= 0) return
        end if
        if (parent_directory(d) == d) exit
        d = parent_directory(d)
    end do
    call empty(cfg)
end subroutine find_config

subroutine load_config(path, fpm, cfg, found, stat, errmsg)
    !! Read the configuration in file `path`: the whole file, or with `fpm` its
    !! `[extra.fformat]` table. `found` is false if an `fpm.toml` has no such table.
    character(len=*), intent(in) :: path                    !! the TOML file
    logical, intent(in) :: fpm                              !! `path` is an `fpm.toml`
    type(config_t), intent(out) :: cfg                      !! the configuration; empty if not `found`
    logical, intent(out) :: found                           !! the file has a configuration
    integer, intent(out) :: stat                            !! nonzero for bad TOML, an unknown key or a bad value
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, with the file name, if `stat` is nonzero

    type(toml_table), allocatable, target :: top
    type(toml_table), pointer :: extra, table
    type(toml_error), allocatable :: error

    stat = 0
    found = .false.
    call empty(cfg)
    call toml_load(top, path, error=error)
    if (allocated(error)) then
        stat = 1
        errmsg = path//': '//error%message
        return
    end if
    if (fpm) then
        call get_value(top, 'extra', extra, requested=.false.)
        if (.not. associated(extra)) return
        call get_value(extra, 'fformat', table, requested=.false.)
        if (.not. associated(table)) return
    else
        table => top
    end if
    found = .true.
    cfg%path = path
    call read_table(table, '')

contains

    recursive subroutine read_table(t, prefix)
        !! Add the settings in table `t`, with keys prefixed by `prefix`.
        type(toml_table), intent(inout) :: t   !! the table to read
        character(len=*), intent(in) :: prefix  !! `''` for the top level, or `spaces.` / `case.`
        type(toml_key), allocatable :: keys(:)
        class(toml_value), pointer :: v
        type(toml_table), pointer :: sub
        character(len=:), allocatable :: key, text
        integer :: k

        call t%get_keys(keys)
        do k = 1, size(keys)
            key = keys(k)%key
            call t%get(key, v)
            select type (v)
            type is (toml_table)
                sub => v
                if (prefix == '' .and. key == 'files') then
                    call read_files(sub)
                else if (prefix == '' .and. (key == 'spaces' .or. key == 'case')) then
                    call read_table(sub, key//'.')
                else
                    call bad('unknown table "'//prefix//key//'"')
                end if
            type is (toml_keyval)
                text = value_text(t, key, v%get_type())
                if (stat /= 0) return
                ! `all` comes first, so that the other entries of the table override it
                if (key == 'all') then
                    cfg%settings = [setting_t(prefix//key, text), cfg%settings]
                else
                    cfg%settings = [cfg%settings, setting_t(prefix//key, text)]
                end if
            class default
                call bad('unexpected value for "'//prefix//key//'"')
            end select
            if (stat /= 0) return
        end do
    end subroutine read_table

    function value_text(t, key, kind) result(text)
        !! A string, integer or boolean value as text.
        type(toml_table), intent(inout) :: t    !! the table holding the value
        character(len=*), intent(in) :: key     !! the value's key in `t`
        integer, intent(in) :: kind             !! the value's type, a `toml_type` member
        character(len=:), allocatable :: text
        integer :: i
        logical :: flag
        text = ''
        if (kind == toml_type%string) then
            call get_value(t, key, text)
        else if (kind == toml_type%int) then
            call get_value(t, key, i)
            text = to_s(i)
        else if (kind == toml_type%boolean) then
            call get_value(t, key, flag)
            text = merge('true ', 'false', flag)
            text = trim(text)
        else
            call bad('expected a string, number or true/false for "'//key//'"')
        end if
    end function value_text

    subroutine read_files(t)
        !! Read the `[files]` table `t` into `cfg%extensions` and `cfg%exclude`.
        type(toml_table), intent(inout) :: t   !! the `[files]` table
        type(toml_key), allocatable :: keys(:)
        integer :: k
        call t%get_keys(keys)
        do k = 1, size(keys)
            select case (keys(k)%key)
            case ('extensions')
                call read_list(t, 'extensions', cfg%extensions)
            case ('exclude')
                call read_list(t, 'exclude', cfg%exclude)
            case default
                call bad('unknown setting "files.'//keys(k)%key//'"')
            end select
            if (stat /= 0) return
        end do
    end subroutine read_files

    subroutine read_list(t, key, list)
        !! An array of strings.
        type(toml_table), intent(inout) :: t                    !! the `[files]` table
        character(len=*), intent(in) :: key                     !! the array's key in `t`
        type(string_t), allocatable, intent(inout) :: list(:)   !! the strings, replacing any earlier list
        type(toml_array), pointer :: arr
        character(len=:), allocatable :: item
        integer :: i, ios
        call get_value(t, key, arr, requested=.false.)
        if (.not. associated(arr)) then
            call bad('expected a list of strings for "files.'//key//'"')
            return
        end if
        if (allocated(list)) deallocate (list)
        allocate (list(0))
        do i = 1, len(arr)
            call get_value(arr, i, item, stat=ios)
            if (ios /= 0 .or. .not. allocated(item)) then
                call bad('expected a list of strings for "files.'//key//'"')
                return
            end if
            list = [list, string_t(item)]
        end do
    end subroutine read_list

    subroutine bad(msg)
        !! Make loading fail with message `msg`, prefixed with the file name.
        character(len=*), intent(in) :: msg   !! what is wrong
        stat = 1
        errmsg = path//': '//msg
    end subroutine bad

end subroutine load_config

subroutine empty(cfg)
    !! A configuration with nothing set.
    type(config_t), intent(out) :: cfg      !! set to have no file, no settings and no `[files]` lists
    cfg%path = ''
    allocate (cfg%settings(0))
end subroutine empty

end module ffmt_config
