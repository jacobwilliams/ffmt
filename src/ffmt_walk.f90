module ffmt_walk
!! Finding the Fortran files under a directory.
!!
!! Files are selected by extension. Entries are excluded by glob patterns: `*` and `?` match within
!! a path component, `**` across components, and `[abc]` / `[!abc]` match one character. A pattern
!! without a `/` is matched against an entry's name, one with a `/` against its path relative to
!! the directory being searched. Links to directories are not followed.
use fregex, only: regex_t, regex, re_escape
use ffmt_fs
use ffmt_strings, only: string_t
implicit none
private

public :: collect_files, glob_to_regex, default_extensions, default_exclude

integer, parameter :: MAX_DEPTH = 100

contains

function default_extensions() result(list)
    !! Extensions of free-form Fortran files.
    type(string_t), allocatable :: list(:)
    list = [string_t('f90'), string_t('F90'), string_t('f95'), string_t('F95'), string_t('f03'), &
            string_t('F03'), string_t('f08'), string_t('F08'), string_t('f18'), string_t('F18')]
end function default_extensions

function default_exclude() result(list)
    !! Hidden files and directories, and fpm's build directory.
    type(string_t), allocatable :: list(:)
    list = [string_t('.*'), string_t('build')]
end function default_exclude

subroutine collect_files(root, extensions, exclude, files)
    !! The files under directory `root` with one of the `extensions` (without the dot) that are not
    !! excluded, sorted by path within each directory.
    character(len=*), intent(in) :: root
    type(string_t), intent(in) :: extensions(:)
    type(string_t), intent(in) :: exclude(:)
    type(string_t), allocatable, intent(out) :: files(:)
    type(regex_t), allocatable :: patterns(:)
    logical, allocatable :: by_path(:)
    integer :: k

    allocate (files(0), patterns(size(exclude)), by_path(size(exclude)))
    do k = 1, size(exclude)
        patterns(k) = regex(glob_to_regex(exclude(k)%s))
        by_path(k) = index(exclude(k)%s, '/') > 0
    end do
    call walk(root, '', 0)

contains

    recursive subroutine walk(dir, rel, depth)
        character(len=*), intent(in) :: dir, rel
        integer, intent(in) :: depth
        type(dir_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: rel_path, path
        integer :: k, stat

        if (depth > MAX_DEPTH) return
        call list_directory(dir, entries, stat)
        if (stat /= 0) return
        call sort_entries(entries)
        do k = 1, size(entries)
            if (len(rel) == 0) then
                rel_path = entries(k)%name
            else
                rel_path = rel//'/'//entries(k)%name
            end if
            if (excluded(entries(k)%name, rel_path)) cycle
            path = child(dir, entries(k)%name)
            select case (entries(k)%kind)
            case (ENTRY_DIR)
                call walk(path, rel_path, depth + 1)
            case (ENTRY_FILE)
                if (has_extension(entries(k)%name)) files = [files, string_t(path)]
            end select
        end do
    end subroutine walk

    logical function excluded(name, rel_path)
        character(len=*), intent(in) :: name, rel_path
        integer :: k
        excluded = .false.
        do k = 1, size(patterns)
            if (by_path(k)) then
                excluded = patterns(k)%fullmatch(rel_path)
            else
                excluded = patterns(k)%fullmatch(name)
            end if
            if (excluded) return
        end do
    end function excluded

    logical function has_extension(name)
        character(len=*), intent(in) :: name
        integer :: k, n
        has_extension = .false.
        do k = 1, size(extensions)
            n = len(extensions(k)%s) + 1
            if (len(name) > n) then
                if (name(len(name) - n + 1:) == '.'//extensions(k)%s) then
                    has_extension = .true.
                    return
                end if
            end if
        end do
    end function has_extension

end subroutine collect_files

pure function child(dir, name) result(path)
    !! `dir/name`, or just `name` in the current directory.
    character(len=*), intent(in) :: dir, name
    character(len=:), allocatable :: path
    if (dir == '.') then
        path = name
    else
        path = join_path(dir, name)
    end if
end function child

subroutine sort_entries(entries)
    !! Sort by name (insertion sort: directories are small).
    type(dir_entry_t), intent(inout) :: entries(:)
    type(dir_entry_t) :: e
    integer :: i, j
    do i = 2, size(entries)
        e = entries(i)
        j = i - 1
        do while (j >= 1)
            if (.not. llt(e%name, entries(j)%name)) exit
            entries(j + 1) = entries(j)
            j = j - 1
        end do
        entries(j + 1) = e
    end do
end subroutine sort_entries

function glob_to_regex(glob) result(re)
    !! A regular expression (for a full match) equivalent to the glob pattern `glob`.
    character(len=*), intent(in) :: glob
    character(len=:), allocatable :: re
    integer :: i, j
    re = ''
    i = 1
    do while (i <= len(glob))
        select case (glob(i:i))
        case ('*')
            if (i < len(glob)) then
                if (glob(i + 1:i + 1) == '*') then
                    if (i + 1 < len(glob)) then
                        if (glob(i + 2:i + 2) == '/') then
                            re = re//'(?:.*/)?'          ! `**/`: any number of directories
                            i = i + 3
                            cycle
                        end if
                    end if
                    re = re//'.*'
                    i = i + 2
                    cycle
                end if
            end if
            re = re//'[^/]*'
        case ('?')
            re = re//'[^/]'
        case ('[')
            j = index(glob(i + 1:), ']')
            if (j > 1) then
                if (glob(i + 1:i + 1) == '!') then
                    re = re//'[^'//glob(i + 2:i + j)
                else
                    re = re//glob(i:i + j)
                end if
                i = i + j + 1
                cycle
            end if
            re = re//'\['
        case default
            re = re//re_escape(glob(i:i))
        end select
        i = i + 1
    end do
end function glob_to_regex

end module ffmt_walk
