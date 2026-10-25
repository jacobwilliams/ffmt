module ffmt_fs
!! Portable file system access that standard Fortran lacks: listing a directory and telling
!! directories from files. Written in Fortran with interfaces to `opendir`, `readdir` and `closedir`,
!! which POSIX systems and MinGW-w64 (gfortran on Windows) provide.
!!
!! The layout of `struct dirent` differs between systems, and gfortran's preprocessor does not say
!! which system it is compiling for, so the layout is found at run time: the offset of `d_name` is
!! where the `.` and `..` entries, which every listing has, show their names. The known layouts
!! are Linux (offset 19), macOS (21), FreeBSD (24), and MinGW or macOS with 32-bit inodes (8).
!! Where the layout has a `d_type` field it is used; otherwise a directory is recognized by opening it.
use, intrinsic :: iso_c_binding
implicit none
private

public :: list_directory, is_directory, join_path, current_directory, absolute_path, parent_directory

integer, parameter, public :: ENTRY_FILE = 1      !! a regular file (or a link to one)
integer, parameter, public :: ENTRY_DIR = 2       !! a directory
integer, parameter, public :: ENTRY_OTHER = 3     !! a link to a directory, or anything else

type, public :: dir_entry_t
    !! One entry of a directory listing.
    character(len=:), allocatable :: name   !! the entry's name, without the directory
    integer :: kind = ENTRY_OTHER           !! `ENTRY_FILE`, `ENTRY_DIR` or `ENTRY_OTHER`
end type dir_entry_t

integer, parameter :: NAME_OFFSETS(*) = [19, 21, 24, 8]   !! offsets of `d_name` to try
integer, parameter :: TYPE_OFFSETS(*) = [18, 20, 20, -1]  !! matching offsets of `d_type` (-1: none)
integer, parameter :: DT_UNKNOWN = 0, DT_DIR = 4, DT_REG = 8, DT_LNK = 10
integer, parameter :: MAX_NAME = 1024

integer, save :: layout = 0      !! index into NAME_OFFSETS once known

interface
    function c_opendir(name) bind(c, name='opendir') result(dir)
        !! C `opendir`: open directory `name` for reading; a null pointer if it cannot be opened.
        import :: c_char, c_ptr
        character(kind=c_char), intent(in) :: name(*)   !! the path, NUL-terminated
        type(c_ptr) :: dir
    end function c_opendir
    function c_readdir(dir) bind(c, name='readdir') result(entry)
        !! C `readdir`: the next `struct dirent` of `dir`, or a null pointer at the end.
        import :: c_ptr
        type(c_ptr), value :: dir                       !! a directory opened by `c_opendir`
        type(c_ptr) :: entry
    end function c_readdir
    function c_closedir(dir) bind(c, name='closedir') result(status)
        !! C `closedir`: close `dir`; 0 on success.
        import :: c_ptr, c_int
        type(c_ptr), value :: dir                       !! a directory opened by `c_opendir`
        integer(c_int) :: status
    end function c_closedir
    function c_getcwd(buf, size) bind(c, name='getcwd') result(p)
        !! C `getcwd`: write the current directory into `buf`, NUL-terminated; a null pointer if
        !! it does not fit or cannot be determined.
        import :: c_char, c_ptr, c_size_t
        character(kind=c_char), intent(out) :: buf(*)   !! receives the path
        integer(c_size_t), value :: size                !! size of `buf` in bytes
        type(c_ptr) :: p
    end function c_getcwd
end interface

contains

subroutine list_directory(path, entries, stat)
    !! The entries of directory `path`, without `.` and `..`, in the order the system returns them.
    !! Links to directories are reported as `ENTRY_OTHER`, so a walk does not follow them. `stat` is
    !! nonzero if the directory cannot be read, or its entries cannot be decoded.
    character(len=*), intent(in) :: path   !! the directory
    type(dir_entry_t), allocatable, intent(out) :: entries(:)   !! the entries; empty if `stat` is nonzero
    integer, intent(out) :: stat        !! 0, or 1 if the listing failed
    type(c_ptr) :: dir, p
    type(dir_entry_t) :: e
    integer :: dtype, status

    allocate (entries(0))
    stat = 1
    if (layout == 0) call find_layout(path)
    if (layout == 0) return
    dir = c_opendir(c_string(path))
    if (.not. c_associated(dir)) return
    stat = 0
    do
        p = c_readdir(dir)
        if (.not. c_associated(p)) exit
        e%name = entry_name(p)
        if (e%name == '.' .or. e%name == '..') cycle
        dtype = DT_UNKNOWN
        if (TYPE_OFFSETS(layout) >= 0) dtype = byte_at(p, TYPE_OFFSETS(layout))
        select case (dtype)
        case (DT_DIR)
            e%kind = ENTRY_DIR
        case (DT_REG)
            e%kind = ENTRY_FILE
        case (DT_LNK)
            e%kind = merge(ENTRY_OTHER, ENTRY_FILE, is_directory(join_path(path, e%name)))
        case (DT_UNKNOWN)
            e%kind = merge(ENTRY_DIR, ENTRY_FILE, is_directory(join_path(path, e%name)))
        case default
            e%kind = ENTRY_OTHER
        end select
        entries = [entries, e]
    end do
    status = c_closedir(dir)
end subroutine list_directory

subroutine find_layout(path)
    !! Find the offset of `d_name` from the `.` and `..` entries of directory `path`.
    character(len=*), intent(in) :: path   !! a directory to list
    type(c_ptr) :: dir, p
    logical :: dot(size(NAME_OFFSETS)), dotdot(size(NAME_OFFSETS))
    integer :: k, status

    dir = c_opendir(c_string(path))
    if (.not. c_associated(dir)) return
    dot = .false.
    dotdot = .false.
    do
        p = c_readdir(dir)
        if (.not. c_associated(p)) exit
        do k = 1, size(NAME_OFFSETS)
            if (byte_at(p, NAME_OFFSETS(k)) /= iachar('.')) cycle
            if (byte_at(p, NAME_OFFSETS(k) + 1) == 0) dot(k) = .true.
            if (byte_at(p, NAME_OFFSETS(k) + 1) == iachar('.') .and. byte_at(p, NAME_OFFSETS(k) + 2) == 0) &
                dotdot(k) = .true.
        end do
    end do
    status = c_closedir(dir)
    do k = 1, size(NAME_OFFSETS)
        if (dot(k) .and. dotdot(k)) then
            layout = k
            return
        end if
    end do
end subroutine find_layout

function entry_name(p) result(name)
    !! The NUL-terminated name in the directory entry at `p`.
    type(c_ptr), intent(in) :: p        !! a `struct dirent` returned by `c_readdir`, with the layout known
    character(len=:), allocatable :: name
    character(kind=c_char), pointer :: raw(:)
    integer :: n, start

    start = NAME_OFFSETS(layout)
    call c_f_pointer(p, raw, [start + MAX_NAME])
    n = 0
    do while (n < MAX_NAME)
        if (raw(start + n + 1) == c_null_char) exit
        n = n + 1
    end do
    allocate (character(len=n) :: name)
    name = transfer(raw(start + 1:start + n), name)
end function entry_name

integer function byte_at(p, offset) result(b)
    !! The byte at `offset` in the memory at `p`, as a number from 0 to 255.
    type(c_ptr), intent(in) :: p        !! start of the memory
    integer, intent(in) :: offset       !! in bytes, 0 for the first byte
    integer(c_int8_t), pointer :: raw(:)
    call c_f_pointer(p, raw, [offset + 1])
    b = iand(int(raw(offset + 1)), 255)
end function byte_at

logical function is_directory(path)
    !! True if `path` is a directory (or a link to one): if it can be opened as a directory.
    character(len=*), intent(in) :: path   !! the path to test
    type(c_ptr) :: dir
    integer :: status
    dir = c_opendir(c_string(path))
    is_directory = c_associated(dir)
    if (is_directory) status = c_closedir(dir)
end function is_directory

function current_directory() result(path)
    !! The current working directory, or `.` if it cannot be determined.
    character(len=:), allocatable :: path
    character(kind=c_char), allocatable :: buf(:)
    integer :: n
    allocate (buf(4096))
    if (.not. c_associated(c_getcwd(buf, int(size(buf), c_size_t)))) then
        path = '.'
        return
    end if
    n = 0
    do while (n < size(buf))
        if (buf(n + 1) == c_null_char) exit
        n = n + 1
    end do
    allocate (character(len=n) :: path)
    path = transfer(buf(1:n), path)
end function current_directory

function absolute_path(path) result(abs)
    !! `path` made absolute by prefixing the current directory, unless it already is
    !! (starts with `/` or `\`, or a drive letter such as `C:`).
    character(len=*), intent(in) :: path   !! a relative or absolute path
    character(len=:), allocatable :: abs
    if (is_absolute(path)) then
        abs = path
    else if (path == '.') then
        abs = current_directory()
    else
        abs = join_path(current_directory(), path)
    end if
end function absolute_path

pure logical function is_absolute(path)
    !! True if `path` starts with `/` or `\`, or has a drive letter such as `C:`.
    character(len=*), intent(in) :: path   !! the path to test
    is_absolute = .false.
    if (len(path) == 0) return
    is_absolute = path(1:1) == '/' .or. path(1:1) == '\'
    if (len(path) >= 2) is_absolute = is_absolute .or. path(2:2) == ':'
end function is_absolute

pure function parent_directory(path) result(parent)
    !! The directory containing `path` (`/a/b` for `/a/b/c`), or `''` if `path` has no parent.
    character(len=*), intent(in) :: path   !! a file or directory path
    character(len=:), allocatable :: parent
    integer :: n, k
    n = len(path)
    do while (n > 1)                         ! ignore trailing separators
        if (index('/\', path(n:n)) == 0) exit
        n = n - 1
    end do
    k = scan(path(1:n), '/\', back=.true.)
    if (k == 0 .or. n <= 1) then
        parent = ''
    else if (k == 1) then
        parent = path(1:1)                   ! the root
    else if (k == 3 .and. path(2:2) == ':') then
        parent = path(1:3)                   ! a drive root, `C:\`
    else
        parent = path(1:k - 1)
    end if
end function parent_directory

pure function join_path(dir, name) result(path)
    !! `dir/name`, without doubling a separator. A trailing `\` or `:` (Windows) also counts.
    character(len=*), intent(in) :: dir, name   !! the directory, and the name to append to it
    character(len=:), allocatable :: path
    if (len(dir) == 0) then
        path = name
    else if (index('/\:', dir(len(dir):len(dir))) > 0) then
        path = dir//name
    else
        path = dir//'/'//name
    end if
end function join_path

pure function c_string(s) result(c)
    !! `s` as a NUL-terminated C string.
    character(len=*), intent(in) :: s   !! the Fortran string
    character(kind=c_char) :: c(len(s) + 1)
    integer :: k
    do k = 1, len(s)
        c(k) = s(k:k)
    end do
    c(len(s) + 1) = c_null_char
end function c_string

end module ffmt_fs
