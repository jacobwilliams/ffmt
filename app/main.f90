program fformat
!! The `fformat` command. See [[ffmt_cli]].
use iso_fortran_env, only: error_unit
use ffmt_cli, only: cli_t, string_t, parse_args, run, EXIT_OK
implicit none

type(cli_t) :: cli
type(string_t), allocatable :: args(:)
character(len=:), allocatable :: errmsg
integer :: k, n, stat

allocate (args(command_argument_count()))
do k = 1, size(args)
    call get_command_argument(k, length=n)
    allocate (character(len=n) :: args(k)%s)
    call get_command_argument(k, args(k)%s)
end do

call parse_args(args, cli, stat, errmsg)
if (stat /= 0) then
    write (error_unit, '(a)') 'fformat: '//errmsg
    write (error_unit, '(a)') 'Try "fformat --help".'
    stop stat, quiet = .true.
end if
stat = run(cli)
if (stat /= EXIT_OK) stop stat, quiet = .true.
end program fformat
