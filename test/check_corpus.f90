program check_corpus
!! Development check over real code: for each file, the tokens must reproduce the source exactly,
!! and the block structure must balance. Prints a diagnostic per problem and a summary.
!!
!! Usage: fpm test check_corpus -- [FILE...] [--files-from LISTFILE]
!! (A list file has one path per line. fpm itself treats arguments starting with `@` as its own
!! response files, so they cannot be used here.)
!! With no arguments it does nothing, so it passes as part of `fpm test`.
use iso_fortran_env, only: output_unit, error_unit, int64
use ffmt_io, only: read_file
use ffmt_tokens
use ffmt_statements
use ffmt_structure
use ffmt_source, only: source_t, parse_source, statement_views
use ffmt_format, only: format_text, FMT_OK
use ffmt_options
implicit none

character(len=:), allocatable :: path, list, text, errmsg
integer :: k, unit, ios, stat
integer :: nfiles, nunreadable, nroundtrip, nunbalanced, ndiags, ninternal, nunstable, nchanged
type(format_options_t) :: styles(3)
integer(int64) :: nbytes, ntokens, nstmts, t0, t1, rate

nfiles = 0
nunreadable = 0
nroundtrip = 0
nunbalanced = 0
ndiags = 0
nbytes = 0
ntokens = 0
nstmts = 0
ninternal = 0
nunstable = 0
nchanged = 0
! the defaults, and two other styles to exercise other code paths
styles(2)%indent = 3
styles(2)%indent_units = .true.
styles(2)%continuation = CONT_INDENT
styles(2)%line_length = 80
styles(2)%relational = REL_SYMBOLS
styles(2)%sort_use = .true.
styles(2)%sort_use_only = .true.
styles(3)%line_length = 100
styles(3)%base_indent = 4
styles(3)%indent_procedures = .false.
styles(3)%keep_column1_comments = .true.
styles(3)%relational = REL_LETTERS
styles(2)%letter_case = case_options_t(CASE_UPPER, CASE_UPPER)
styles(3)%letter_case = case_options_t(CASE_LOWER, CASE_LOWER)
styles(2)%spaces = whitespace_options_t(WS_ADD, WS_ADD, WS_ADD, WS_ADD, WS_ADD, WS_ADD, WS_ADD, WS_ADD, &
                                        WS_ADD, WS_ADD, WS_ADD, WS_ADD, WS_ADD, WS_ADD)
styles(3)%spaces = whitespace_options_t(WS_REMOVE, WS_REMOVE, WS_REMOVE, WS_REMOVE, WS_REMOVE, WS_REMOVE, &
                                        WS_REMOVE, WS_REMOVE, WS_REMOVE, WS_REMOVE, WS_REMOVE, WS_REMOVE, &
                                        WS_REMOVE, WS_REMOVE)
call system_clock(t0, rate)

k = 0
do while (k < command_argument_count())
    k = k + 1
    list = argument(k)
    if (list == '--files-from') then
        k = k + 1
        list = argument(k)
        open (newunit=unit, file=list, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(a)') 'cannot open '//list
            error stop 1
        end if
        do
            call read_line(unit, path, ios)
            if (ios /= 0) exit
            if (len(path) > 0) call check_file(path)
        end do
        close (unit)
    else
        call check_file(list)
    end if
end do

if (nfiles == 0) stop
call system_clock(t1)
write (output_unit, '(a)') repeat('-', 60)
write (output_unit, '(i0, a, i0, a, i0, a, i0, a, f0.2, a)') nfiles, ' files, ', nbytes, ' bytes, ', ntokens, &
    ' tokens, ', nstmts, ' items in ', real(t1 - t0)/real(rate), ' s'
write (output_unit, '(a, i0)') 'unreadable files:          ', nunreadable
write (output_unit, '(a, i0)') 'round-trip failures:       ', nroundtrip
write (output_unit, '(a, i0, a, i0, a)') 'files with diagnostics:    ', nunbalanced, ' (', ndiags, ' diagnostics)'
write (output_unit, '(a, i0)') 'changed by formatting:     ', nchanged
write (output_unit, '(a, i0)') 'internal errors:           ', ninternal
write (output_unit, '(a, i0)') 'not idempotent:            ', nunstable
if (nroundtrip > 0 .or. ninternal > 0 .or. nunstable > 0) error stop 1

contains

subroutine check_file(path)
    character(len=*), intent(in) :: path
    type(source_t) :: source
    type(structure_t) :: st
    type(diagnostic_t), allocatable :: diags(:)
    integer :: d

    nfiles = nfiles + 1
    call read_file(path, text, stat, errmsg)
    if (stat /= 0) then
        nunreadable = nunreadable + 1
        write (output_unit, '(a)') path//': cannot read: '//errmsg
        return
    end if
    nbytes = nbytes + len(text)
    call parse_source(text, source)
    ntokens = ntokens + source%tokens%n
    if (detokenize(text, source%tokens) /= text) then
        nroundtrip = nroundtrip + 1
        write (output_unit, '(a)') path//': tokens do not reproduce the source'
        return
    end if
    nstmts = nstmts + source%stmts%n
    call analyze_structure(source, statement_views(source), st, diags)
    if (size(diags) > 0) nunbalanced = nunbalanced + 1
    ndiags = ndiags + size(diags)
    do d = 1, size(diags)
        write (output_unit, '(a, ":", i0, ": ", a)') path, diags(d)%line, diags(d)%message
    end do
    do d = 1, size(styles)
        call check_formatting(path, styles(d), d == 1)
    end do
end subroutine check_file

subroutine check_formatting(path, opts, count_changes)
    !! Format with `opts`: the code must be unchanged (or `format_text` reports an internal error)
    !! and formatting the result again must not change it.
    character(len=*), intent(in) :: path
    type(format_options_t), intent(in) :: opts
    logical, intent(in) :: count_changes
    character(len=:), allocatable :: once, twice, msg
    integer :: stat, first

    call format_text(text, opts, once, stat, msg)
    if (stat /= FMT_OK) then
        ninternal = ninternal + 1
        write (output_unit, '(a)') path//': '//msg
        return
    end if
    if (count_changes .and. once /= text) nchanged = nchanged + 1
    call format_text(once, opts, twice, stat, msg)
    if (twice /= once .or. len(twice) /= len(once)) then
        nunstable = nunstable + 1
        first = 1
        do while (first <= min(len(once), len(twice)))
            if (once(first:first) /= twice(first:first)) exit
            first = first + 1
        end do
        write (output_unit, '(a, i0)') path//': second formatting pass differs at line ', &
            count_lines(once(1:first - 1)) + 1
    end if
end subroutine check_formatting

pure integer function count_lines(s) result(n)
    character(len=*), intent(in) :: s
    integer :: k
    n = 0
    do k = 1, len(s)
        if (s(k:k) == achar(10)) n = n + 1
    end do
end function count_lines

function argument(k) result(arg)
    integer, intent(in) :: k
    character(len=:), allocatable :: arg
    integer :: n
    call get_command_argument(k, length=n)
    allocate (character(len=n) :: arg)
    call get_command_argument(k, arg)
end function argument

subroutine read_line(unit, line, ios)
    integer, intent(in) :: unit
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: ios
    character(len=1024) :: buf
    integer :: sz
    line = ''
    do
        read (unit, '(a)', advance='no', iostat=ios, size=sz) buf
        line = line//buf(1:sz)
        if (ios /= 0) exit
    end do
    if (is_iostat_eor(ios)) ios = 0
    if (len(line) > 0) then
        if (line(len(line):len(line)) == achar(13)) line = line(1:len(line) - 1)
    end if
end subroutine read_line

end program check_corpus
