"""System Call extension: SYSCALL entry, RETSC return, and the control-event faults.

Programs run with Dynamic Hardware + System Call enabled so privilege/interrupt stay off and the
processor remains in Machine Mode. The fault/syscall handlers report the low byte of CAUSE.
"""

import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")

SYS = ("dynamichw", "syscall")


def test_syscall_enters_handler_and_sets_cause() -> None:
    # HALT the CAUSE low byte (0x01): a SYSCALL-as-HALT bug would emit RA (0x07), not 0x01.
    r: dict = run_program("""
        set ra 0x07
        syscall ra
    _handler:
        rsr cause ra rb
        halt rb
    """, extns=SYS, syscall="_handler")
    assert r["halt_code"] == 0x01


def test_syscall_sets_retsc_to_following_instruction() -> None:
    # RETSC is the address of the instruction after SYSCALL (0xC004 -> low byte 0x04).
    r: dict = run_program("""
        set ra 0x07
        syscall ra
    _handler:
        rsr retsc ra rb
        halt rb
    """, extns=SYS, syscall="_handler")
    assert r["halt_code"] == 0x04


def test_syscall_return_leaves_context() -> None:
    r: dict = run_program("""
        set ra 0x07
        syscall ra
        set rc 0x55
        halt rc
    _handler:
        rets
    """, extns=SYS, syscall="_handler")
    assert r["halt_code"] == 0x55


def test_conditional_context_return_faults() -> None:
    # A JMP to RETSC with a non-ALWAYS comparison is a conditional context return (CAUSE 0x0B).
    r: dict = run_program("""
        set ra 0x07
        syscall ra
    _handler:
        sub r0 r0 r0
        jmp equal retsc
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=SYS, syscall="_handler", fault="_fault")
    assert r["halt_code"] == 0x0B


def test_improper_return_context_faults() -> None:
    # RETS with no active system-call context is an improper return (CAUSE 0x03).
    r: dict = run_program("""
        rets
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=SYS, fault="_fault")
    assert r["halt_code"] == 0x03


def test_nested_syscall_faults() -> None:
    r: dict = run_program("""
        set ra 0x07
        syscall ra
    _handler:
        syscall rb
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=SYS, syscall="_handler", fault="_fault")
    assert r["halt_code"] == 0x13


def test_syscall_behaves_as_halt_when_extension_disabled() -> None:
    # Assembled with -e syscall so SYSCALL parses, but run with no extensions: it acts as HALT.
    r: dict = run_program("""
        set ra 0x2a
        syscall ra
    """, extns=(), as_extns=("syscall",))
    assert r["halted"] == 1 and r["halt_code"] == 0x2A
