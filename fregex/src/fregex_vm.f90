module fregex_vm
!! Backtracking virtual machine that runs a compiled program against a string.
!!
!! Choice points and the old values of captures and registers are kept on one stack. On failure
!! the stack is popped: saved values are restored (undoing the failed path) until a choice point
!! is found to resume from. Lookarounds and atomic groups run their body in a nested call.
use iso_fortran_env, only: int64
use fregex_program
use fregex_string, only: byte_at, to_lower_code, is_word_code
implicit none
private

public :: vm_exec

integer, parameter, public :: MODE_SEARCH = 0     !! find the leftmost match at or after `start`
integer, parameter, public :: MODE_MATCH = 1      !! match must start at `start`
integer, parameter, public :: MODE_FULLMATCH = 2  !! match must start at `start` and end at the end of the string

integer, parameter :: BT_BRANCH = 1   !! resume at pc x, position y
integer, parameter :: BT_CAP = 2      !! restore capture slot x to y
integer, parameter :: BT_REG = 3      !! restore register x to y

type :: vm_t
    integer, allocatable :: caps(:)        !! (0:2*ngroups+1), positions; 0 if unset
    integer, allocatable :: regs(:)
    integer, allocatable :: stack(:, :)    !! (3, capacity): kind, x, y
    integer :: top = 0
    integer(int64) :: steps = 0
    integer(int64) :: step_limit = 0
    logical :: overflow = .false.
    integer :: forbid_empty_at = 0         !! reject an empty match at this position
    logical :: require_end = .false.
end type vm_t

contains

subroutine vm_exec(prog, s, start, mode, forbid_empty_at, step_limit, found, caps, overflow)
    !! Run `prog` on `s`. On success `caps` holds the capture positions: slot `2k` is the first
    !! index of group k, slot `2k+1` is one past its last index, and 0 means the group did not match.
    !!
    !! `overflow` is set if the step limit was exceeded before an answer was found.
    type(program_t), intent(in) :: prog
    character(len=*), intent(in) :: s
    integer, intent(in) :: start
    integer, intent(in) :: mode
    integer, intent(in) :: forbid_empty_at
    integer(int64), intent(in) :: step_limit
    logical, intent(out) :: found
    integer, allocatable, intent(out) :: caps(:)
    logical, intent(out) :: overflow

    type(vm_t) :: vm
    integer :: st, last, sp_end

    found = .false.
    overflow = .false.
    allocate (vm%caps(0:2*prog%ngroups + 1), vm%regs(max(1, prog%nregs)), vm%stack(3, 64))
    vm%step_limit = step_limit
    vm%forbid_empty_at = forbid_empty_at
    vm%require_end = mode == MODE_FULLMATCH

    last = len(s) + 1
    if (mode /= MODE_SEARCH) last = start
    if (prog%anchored) last = min(last, 1)

    do st = max(1, start), last
        if (prog%use_first) then
            if (.not. prog%first%has(byte_at(s, st))) cycle
        end if
        vm%caps = 0
        vm%regs = 0
        vm%top = 0
        if (run(prog, s, vm, 1, st, sp_end)) then
            found = .true.
            call move_alloc(vm%caps, caps)
            return
        end if
        if (vm%overflow) then
            overflow = .true.
            return
        end if
    end do
end subroutine vm_exec

recursive function run(prog, s, vm, pc0, sp0, sp_out) result(ok)
    !! Run from instruction `pc0` at position `sp0` until OP_MATCH or OP_SUCCEED (true, with the
    !! final position in `sp_out`) or until every alternative is exhausted (false). Only stack
    !! entries pushed by this call are popped, so nested calls leave the caller's state alone.
    type(program_t), intent(in) :: prog
    character(len=*), intent(in) :: s
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: pc0, sp0
    integer, intent(out) :: sp_out
    logical :: ok

    integer :: base, pc, sp, n, c, k, t0, st, lo, hi, sub_sp, gs, ge, glen
    logical :: fail, sub_ok, negated

    n = len(s)
    base = vm%top
    pc = pc0
    sp = sp0
    sp_out = sp0
    ok = .false.

    do
        vm%steps = vm%steps + 1
        if (vm%steps > vm%step_limit) then
            vm%overflow = .true.
            return
        end if

        fail = .false.
        select case (prog%code(0, pc))
        case (OP_CHAR)
            if (byte_at(s, sp) == prog%code(1, pc)) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_CHARI)
            if (to_lower_code(byte_at(s, sp)) == prog%code(1, pc)) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_ANY)
            c = byte_at(s, sp)
            if (c >= 0 .and. c /= 10) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_ANYNL)
            if (sp <= n) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_SET)
            if (prog%sets(prog%code(1, pc))%has(byte_at(s, sp))) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_SPLIT)
            call push(vm, BT_BRANCH, prog%code(2, pc), sp)
            pc = prog%code(1, pc)
        case (OP_JMP)
            pc = prog%code(1, pc)
        case (OP_SAVE)
            k = prog%code(1, pc)
            call push(vm, BT_CAP, k, vm%caps(k))
            vm%caps(k) = sp
            pc = pc + 1
        case (OP_MARK)
            k = prog%code(1, pc)
            call push(vm, BT_REG, k, vm%regs(k))
            vm%regs(k) = sp
            pc = pc + 1
        case (OP_CHECK_EXIT)
            if (sp == vm%regs(prog%code(1, pc))) then
                pc = prog%code(2, pc)
            else
                pc = pc + 1
            end if
        case (OP_BOL, OP_BOL_M, OP_EOL, OP_EOL_M, OP_BOS, OP_EOS, OP_WORDB, OP_NWORDB)
            if (check_assertion(prog%code(0, pc), s, sp)) then
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_BACKREF)
            k = prog%code(1, pc)
            gs = vm%caps(2*k)
            ge = vm%caps(2*k + 1)
            fail = .true.
            if (gs > 0 .and. ge > 0) then
                glen = ge - gs
                if (sp + glen - 1 <= n) then
                    if (prog%code(2, pc) == 1) then
                        fail = .false.
                        do c = 0, glen - 1
                            if (to_lower_code(byte_at(s, sp + c)) /= to_lower_code(byte_at(s, gs + c))) then
                                fail = .true.
                                exit
                            end if
                        end do
                    else
                        fail = s(sp:sp + glen - 1) /= s(gs:ge - 1)
                    end if
                end if
            end if
            if (.not. fail) then
                sp = sp + glen
                pc = pc + 1
            end if
        case (OP_LOOKAHEAD)
            t0 = vm%top
            negated = prog%code(2, pc) == 1
            sub_ok = run(prog, s, vm, pc + 1, sp, sub_sp)
            if (vm%overflow) return
            if (sub_ok .neqv. negated) then
                if (sub_ok) call compact(vm, t0)
                pc = prog%code(1, pc)
            else
                if (sub_ok) call unwind(vm, t0)
                fail = .true.
            end if
        case (OP_LOOKBEHIND)
            t0 = vm%top
            negated = prog%code(2, pc) == 1
            k = prog%code(5, pc)
            call push(vm, BT_REG, k, vm%regs(k))
            vm%regs(k) = sp
            ! try every start position allowed by the body's width, nearest first
            hi = sp - prog%code(3, pc)
            if (prog%code(4, pc) == UNBOUNDED) then
                lo = 1
            else
                lo = max(1, sp - prog%code(4, pc))
            end if
            sub_ok = .false.
            do st = hi, lo, -1
                sub_ok = run(prog, s, vm, pc + 1, st, sub_sp)
                if (vm%overflow) return
                if (sub_ok) exit
            end do
            if (sub_ok .neqv. negated) then
                if (sub_ok) then
                    call compact(vm, t0)
                else
                    call unwind(vm, t0)
                end if
                pc = prog%code(1, pc)
            else
                call unwind(vm, t0)
                fail = .true.
            end if
        case (OP_ENDAT)
            if (sp == vm%regs(prog%code(1, pc))) then
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_ATOMIC)
            t0 = vm%top
            sub_ok = run(prog, s, vm, pc + 1, sp, sub_sp)
            if (vm%overflow) return
            if (sub_ok) then
                call compact(vm, t0)
                sp = sub_sp
                pc = prog%code(1, pc)
            else
                fail = .true.
            end if
        case (OP_SUCCEED)
            ok = .true.
            sp_out = sp
            return
        case (OP_MATCH)
            if (vm%require_end .and. sp /= n + 1) then
                fail = .true.
            else if (sp == vm%forbid_empty_at .and. vm%caps(0) == sp) then
                fail = .true.
            else
                ok = .true.
                sp_out = sp
                return
            end if
        end select

        if (fail) then
            ! backtrack: undo saved values until a choice point is found
            do
                if (vm%top <= base) return
                k = vm%top
                vm%top = vm%top - 1
                select case (vm%stack(1, k))
                case (BT_BRANCH)
                    pc = vm%stack(2, k)
                    sp = vm%stack(3, k)
                    exit
                case (BT_CAP)
                    vm%caps(vm%stack(2, k)) = vm%stack(3, k)
                case (BT_REG)
                    vm%regs(vm%stack(2, k)) = vm%stack(3, k)
                end select
            end do
        end if
    end do
end function run

pure logical function check_assertion(op, s, sp) result(ok)
    integer, intent(in) :: op
    character(len=*), intent(in) :: s
    integer, intent(in) :: sp
    integer :: n

    n = len(s)
    select case (op)
    case (OP_BOL, OP_BOS)
        ok = sp == 1
    case (OP_BOL_M)
        ok = sp == 1 .or. byte_at(s, sp - 1) == 10
    case (OP_EOL)
        ok = sp == n + 1 .or. (sp == n .and. byte_at(s, sp) == 10)
    case (OP_EOL_M)
        ok = sp == n + 1 .or. byte_at(s, sp) == 10
    case (OP_EOS)
        ok = sp == n + 1
    case (OP_WORDB)
        ok = is_word_code(byte_at(s, sp - 1)) .neqv. is_word_code(byte_at(s, sp))
    case default   ! OP_NWORDB; like Python, never matches in an empty string
        ok = n > 0 .and. (is_word_code(byte_at(s, sp - 1)) .eqv. is_word_code(byte_at(s, sp)))
    end select
end function check_assertion

pure subroutine push(vm, kind, x, y)
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: kind, x, y
    integer, allocatable :: tmp(:, :)

    if (vm%top == size(vm%stack, 2)) then
        allocate (tmp(3, 2*vm%top))
        tmp(:, 1:vm%top) = vm%stack(:, 1:vm%top)
        call move_alloc(tmp, vm%stack)
    end if
    vm%top = vm%top + 1
    vm%stack(1, vm%top) = kind
    vm%stack(2, vm%top) = x
    vm%stack(3, vm%top) = y
end subroutine push

pure subroutine unwind(vm, t0)
    !! Pop everything above `t0`, restoring saved values.
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: t0
    integer :: k
    do k = vm%top, t0 + 1, -1
        select case (vm%stack(1, k))
        case (BT_CAP)
            vm%caps(vm%stack(2, k)) = vm%stack(3, k)
        case (BT_REG)
            vm%regs(vm%stack(2, k)) = vm%stack(3, k)
        end select
    end do
    vm%top = t0
end subroutine unwind

pure subroutine compact(vm, t0)
    !! Drop the choice points above `t0` but keep the saved values, so that a successful
    !! lookaround or atomic group cannot be re-entered, yet its effects can still be undone.
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: t0
    integer :: k, j
    j = t0
    do k = t0 + 1, vm%top
        if (vm%stack(1, k) /= BT_BRANCH) then
            j = j + 1
            vm%stack(:, j) = vm%stack(:, k)
        end if
    end do
    vm%top = j
end subroutine compact

end module fregex_vm
