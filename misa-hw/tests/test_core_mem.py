import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")


def test_round_trip_store_then_load_reg_form() -> None:
    # Pointer {RA,RB} = 0x0210; store RC there, clobber RC, load it back.
    r: dict = run_program("""
        set ra 0x02
        set rb 0x10
        set rc 0x5a
        st rc ra 0
        set rc 0x00
        ld rd ra 0
        halt rd
    """)
    assert r["halt_code"] == 0x5A


def test_store_with_negative_offset_sign_extends() -> None:
    # Base {0x02,0x11}=0x0211; ST at -1 -> 0x0210; load 0x0210 via base {0x02,0x10}.
    r: dict = run_program("""
        set ra 0x02
        set rb 0x11
        set rc 0x7e
        st rc ra -1
        set rb 0x10
        ld rd ra 0
        halt rd
    """)
    assert r["halt_code"] == 0x7E


def test_base_pair_low_register_is_rsb_plus_one() -> None:
    # Base register RSCRATCH0 (0xE); the low byte comes from the next register, RSCRATCH1 (0xF).
    r: dict = run_program("""
        set rscratch0 0x03
        set rscratch1 0x20
        set ra 0x33
        st ra rscratch0 0
        set ra 0x00
        ld rb rscratch0 0
        halt rb
    """)
    assert r["halt_code"] == 0x33


def test_stack_push_then_pop() -> None:
    # Initialize SADDR to 0x01FF, push two bytes, pop the last one.
    r: dict = run_program("""
        set2 rscratch 0x01ff
        wsr saddr rscratch
        set ra 0xaa
        set rb 0xbb
        push ra
        push rb
        pop rc
        halt rc
    """)
    assert r["halt_code"] == 0xBB
