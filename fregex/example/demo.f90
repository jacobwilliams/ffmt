program demo
use fregex
implicit none
type(regex_t) :: re
type(match_t) :: m

re = regex('(?P<name>\w+)\s*::\s*(?P<vars>.*)', RE_IGNORECASE)
if (re%search('integer :: i, j', m)) then
    print *, m%group('name')                  ! integer
    print *, m%group('vars'), m%start('vars') ! i, j   12
end if

print *, re_sub('\s*([=+])\s*', 'x=y+1', ' \1 ')  ! x = y + 1
end program demo
