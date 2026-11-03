import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")


def test_jal_taken_reaches_target() -> None:
    # An always-taken JAL skips the trap and reaches the target label.
    r: dict = run_program("""
        jali always _t
        halt r0
    _t:
        set rc 0x77
        halt rc
    """)
    assert r["halt_code"] == 0x77


def test_jal_not_taken_falls_through() -> None:
    # Z is clear at reset, so an EQUAL JAL is not taken and execution falls through.
    r: dict = run_program("""
        jali equal _t
        set rc 0x11
        halt rc
    _t:
        set rc 0xee
        halt rc
    """)
    assert r["halt_code"] == 0x11


def test_call_links_and_ret_returns() -> None:
    # CALL sets RADDR to the instruction after it; RET (JMP ALWAYS RADDR) returns there.
    r: dict = run_program("""
        call _sub
        halt rc
    _sub:
        set rc 0x42
        ret
    """)
    assert r["halt_code"] == 0x42


def test_jmp_reg_taken() -> None:
    r: dict = run_program("""
        jmpi always _t
        halt r0
    _t:
        set rc 0x77
        halt rc
    """)
    assert r["halt_code"] == 0x77


def test_jmp_reg_not_taken() -> None:
    r: dict = run_program("""
        jmpi equal _t
        set rc 0x11
        halt rc
    _t:
        set rc 0xee
        halt rc
    """)
    assert r["halt_code"] == 0x11


def test_wsr_flags_affects_branch() -> None:
    # Writing FLAGS to set Z=1 makes an EQUAL JMP taken.
    r: dict = run_program("""
        set rc 0x00
        set rd 0x01
        wsr flags rc rd
        jmpi equal _taken
        set re 0xaa
        halt re
    _taken:
        set re 0x5a
        halt re
    """)
    assert r["halt_code"] == 0x5A
