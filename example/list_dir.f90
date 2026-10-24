program list_dir
!! Print the entries of a directory with their kinds (a small demonstration of [[ffmt_fs]]).
use ffmt_fs
implicit none
type(dir_entry_t), allocatable :: entries(:)
character(len=4096) :: path
integer :: k, stat
character(len=*), parameter :: kinds(3) = [character(len=5) :: 'file', 'dir', 'other']

call get_command_argument(1, path)
if (len_trim(path) == 0) path = '.'
call list_directory(trim(path), entries, stat)
if (stat /= 0) error stop 'cannot read directory'
do k = 1, size(entries)
    print '(a, 1x, a)', kinds(entries(k)%kind), entries(k)%name
end do
end program list_dir
