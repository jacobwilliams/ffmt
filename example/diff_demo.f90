program diff_demo
!! Print the unified diff of two files (a small demonstration of [[ffmt_diff]]).
use ffmt_diff, only: unified_diff
use ffmt_io, only: read_file
implicit none
character(len=:), allocatable :: a, b, errmsg
character(len=4096) :: fa, fb
integer :: stat
call get_command_argument(1, fa)
call get_command_argument(2, fb)
call read_file(trim(fa), a, stat, errmsg)
call read_file(trim(fb), b, stat, errmsg)
write (*, '(a)', advance='no') unified_diff(a, b, trim(fb))
end program diff_demo
