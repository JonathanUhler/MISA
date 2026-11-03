import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")


def test_set_then_halt_emits_register() -> None:
    r: dict = run_program("""
        set ra 0x2a
        halt ra
    """)
    assert r["halted"] == 1
    assert r["halt_code"] == 0x2A


def test_set_other_register_and_immediate() -> None:
    r: dict = run_program("""
        set rb 0xff
        halt rb
    """)
    assert r["halted"] == 1
    assert r["halt_code"] == 0xFF
