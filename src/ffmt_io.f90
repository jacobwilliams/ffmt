module ffmt_io
!! Reading and writing whole files as bytes, so that line endings are preserved on every platform.
implicit none
private

public :: read_file, write_file, read_stdin

contains

subroutine read_file(path, text, stat, errmsg)
    !! All bytes of file `path`.
    character(len=*), intent(in) :: path   !! the file to read
    character(len=:), allocatable, intent(out) :: text      !! the contents; `''` if the file cannot be opened
    integer, intent(out) :: stat                            !! `iostat` of the open or read: nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg    !! the system's message, if `stat` is nonzero
    integer :: unit, nbytes
    character(len=256) :: msg

    text = ''
    open (newunit=unit, file=path, access='stream', form='unformatted', status='old', &
          action='read', iostat=stat, iomsg=msg)
    if (stat /= 0) then
        errmsg = trim(msg)
        return
    end if
    inquire (unit=unit, size=nbytes)
    if (nbytes > 0) then
        deallocate (text)
        allocate (character(len=nbytes) :: text)
        read (unit, iostat=stat, iomsg=msg) text
        if (stat /= 0) errmsg = trim(msg)
    end if
    close (unit)
end subroutine read_file

subroutine write_file(path, text, stat, errmsg)
    !! Replace the contents of file `path` (created if needed) with the bytes of `text`.
    character(len=*), intent(in) :: path   !! the file to write
    character(len=*), intent(in) :: text   !! the new contents
    integer, intent(out) :: stat                            !! `iostat` of the open or write: nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg    !! the system's message, if `stat` is nonzero
    integer :: unit
    character(len=256) :: msg

    open (newunit=unit, file=path, access='stream', form='unformatted', status='replace', &
          action='write', iostat=stat, iomsg=msg)
    if (stat /= 0) then
        errmsg = trim(msg)
        return
    end if
    write (unit, iostat=stat, iomsg=msg) text
    if (stat /= 0) errmsg = trim(msg)
    close (unit)
end subroutine write_file

subroutine read_stdin(text)
    !! All of standard input. Lines are read as text, so each line ends with a line feed, except a
    !! last line without one; carriage returns of CRLF line endings are kept as part of the line.
    use, intrinsic :: iso_fortran_env, only: input_unit
    use ffmt_strings, only: string_builder_t
    character(len=:), allocatable, intent(out) :: text      !! the input
    type(string_builder_t) :: buf
    character(len=4096) :: chunk
    integer :: ios, n

    do
        read (input_unit, '(a)', advance='no', iostat=ios, size=n) chunk
        call buf%append(chunk(1:n))
        if (is_iostat_eor(ios)) then
            call buf%append(achar(10))
        else if (ios /= 0) then
            exit                                ! end of input
        end if
    end do
    text = buf%get()
end subroutine read_stdin

end module ffmt_io
