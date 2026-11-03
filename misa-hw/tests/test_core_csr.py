import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")


def test_wsr_then_rsr_high_byte_to_rs1() -> None:
    # WSR SADDR = 0x1234; RSR reads the high byte (0x12) into the first register.
    r: dict = run_program("""
        set ra 0x12
        set rb 0x34
        wsr saddr ra rb
        rsr saddr rc rd
        halt rc
    """)
    assert r["halt_code"] == 0x12


def test_wsr_then_rsr_low_byte_to_rs2() -> None:
    r: dict = run_program("""
        set ra 0x12
        set rb 0x34
        wsr saddr ra rb
        rsr saddr rc rd
        halt rd
    """)
    assert r["halt_code"] == 0x34


def test_wsr_flags_direct_write_then_rsr() -> None:
    # Writing FLAGS directly (not via the ALU) must read back through RSR.
    r: dict = run_program("""
        set ra 0x00
        set rb 0x0a
        wsr flags ra rb
        rsr flags rc rd
        halt rd
    """)
    assert r["halt_code"] == 0x0A
