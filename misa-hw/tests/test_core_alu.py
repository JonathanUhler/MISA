import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")


def test_add() -> None:
    r: dict = run_program("""
        set ra 0x05
        set rb 0x03
        add rc ra rb
        halt rc
    """)
    assert r["halt_code"] == 0x08


def test_adc_uses_prior_carry_and_set_preserves_flags() -> None:
    # ADD RA+RB overflows (-> 0x00, C=1); the non-ALU SET must not clear flags; ADC R0+R0+C = 0x01.
    r: dict = run_program("""
        set ra 0xff
        set rb 0x01
        add rc ra rb
        set rd 0x07
        adc re r0 r0
        halt re
    """)
    assert r["halt_code"] == 0x01


def test_rrc_chain() -> None:
    # 0x02 >>1 -> 0x01 (C=0); 0x01 >>1 -> 0x00 (C=1); C>>in -> 0x80.
    r: dict = run_program("""
        set ra 0x02
        rrc rb ra
        rrc rc rb
        rrc rd rc
        halt rd
    """)
    assert r["halt_code"] == 0x80


def test_write_to_r0_discarded() -> None:
    r: dict = run_program("""
        set ra 0x05
        set rb 0x03
        add r0 ra rb
        add rc r0 rb
        halt rc
    """)
    assert r["halt_code"] == 0x03


def test_and_then_or() -> None:
    r: dict = run_program("""
        set ra 0xf0
        set rb 0x0f
        and rc ra rb
        or rd ra rb
        halt rd
    """)
    assert r["halt_code"] == 0xFF
