module ffmt_strings
!! String helpers.
implicit none
private

public :: utf8_width, to_s

type, public :: string_t
    !! A variable-length string, for arrays of strings of different lengths.
    character(len=:), allocatable :: s
end type string_t

type, public :: string_builder_t
    !! Builds a string by appending pieces in amortized constant time.
    private
    character(len=:), allocatable :: buf
    integer :: n = 0
contains
    procedure :: append => builder_append
    procedure :: get => builder_get
end type string_builder_t

contains

pure integer function utf8_width(text) result(w)
    !! Number of characters (code points) in UTF-8 `text`: bytes that are not continuation bytes.
    character(len=*), intent(in) :: text
    integer :: k, c
    w = 0
    do k = 1, len(text)
        c = iand(ichar(text(k:k)), 255)
        if (c < 128 .or. c >= 192) w = w + 1
    end do
end function utf8_width

pure function to_s(i) result(s)
    !! Integer to string, without padding.
    integer, intent(in) :: i
    character(len=:), allocatable :: s
    character(len=16) :: buf
    write (buf, '(i0)') i
    s = trim(buf)
end function to_s

pure subroutine builder_append(self, str)
    class(string_builder_t), intent(inout) :: self
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: tmp

    if (len(str) == 0) return
    if (.not. allocated(self%buf)) allocate (character(len=max(256, len(str))) :: self%buf)
    if (self%n + len(str) > len(self%buf)) then
        allocate (character(len=max(2*len(self%buf), self%n + len(str))) :: tmp)
        tmp(1:self%n) = self%buf(1:self%n)
        call move_alloc(tmp, self%buf)
    end if
    self%buf(self%n + 1:self%n + len(str)) = str
    self%n = self%n + len(str)
end subroutine builder_append

pure function builder_get(self) result(str)
    class(string_builder_t), intent(in) :: self
    character(len=:), allocatable :: str
    if (allocated(self%buf)) then
        str = self%buf(1:self%n)
    else
        str = ''
    end if
end function builder_get

end module ffmt_strings
