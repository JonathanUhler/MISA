"""Dynamic Hardware: EXTNS is writable, bit 0 pinned, writes ignored when Dynamic Hardware is off.

Observed through CSR availability: RSR RETSC is legal only while EXTNS.syscall is set, so toggling
the syscall bit via WSR EXTNS changes whether a following RSR RETSC faults as illegal (CAUSE 0x09).
"""

import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")

ALL = ("dynamichw", "syscall", "privilege", "interrupt")


def test_disabling_syscall_takes_effect_next_instruction() -> None:
    # EXTNS=0x0F; clearing the syscall bit makes the next RSR RETSC fault as illegal.
    r: dict = run_program("""
        set rc 0x00
        set rd 0x0d
        wsr extns rc rd
        rsr retsc ra rb
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=ALL, fault="_fault")
    assert r["halt_code"] == 0x09


def test_bit0_cannot_be_cleared() -> None:
    # Trying to clear bit 0 leaves Dynamic Hardware on, so a later WSR EXTNS still works.
    r: dict = run_program("""
        set rc 0x00
        set rd 0x0e
        wsr extns rc rd
        set rd 0x0d
        wsr extns rc rd
        rsr retsc ra rb
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=ALL, fault="_fault")
    assert r["halt_code"] == 0x09


def test_write_ignored_when_dynamic_hardware_off() -> None:
    # EXTNS=0x00: WSR EXTNS cannot enable syscall, so RSR RETSC stays illegal. The program is
    # assembled with -e syscall so RSR RETSC parses, but runs with no extensions enabled.
    r: dict = run_program("""
        set rc 0x00
        set rd 0x02
        wsr extns rc rd
        rsr retsc ra rb
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=(), as_extns=("syscall",), fault="_fault")
    assert r["halt_code"] == 0x09
