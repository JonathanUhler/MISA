import pytest

from conftest import make_image, put16, run_program, run_until_halt, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")


def test_illegal_instruction_sets_cause_and_vectors() -> None:
    # A raw illegal RSR (CSR id 0x6) faults; the handler reports CAUSE's low byte (0x09 = illegal).
    r: dict = run_program("""
        .array 0x0c 0x60
    _fault:
        rsr cause ra rb
        halt rb
    """, fault="_fault")
    assert r["halt_code"] == 0x09


def test_illegal_instruction_discards_partial_result() -> None:
    # RA = 0x5A, then an illegal RSR whose rs1 is RA; if preempted, RA keeps 0x5A.
    r: dict = run_program("""
        set ra 0x5a
        .array 0x1c 0x62
    _fault:
        halt ra
    """, fault="_fault")
    assert r["halt_code"] == 0x5A


def test_pc_overflow_faults() -> None:
    # Built as a raw image: the reset-vector word 0xC009 doubles as a sequential SET at 0xFFFE, so
    # jumping there and advancing wraps PC past 0xFFFF. This unaligned-execution corner has no clean
    # assembly form, so the image is laid out by hand.
    img: list = make_image()
    put16(img, 0xFFFE, 0xC009)  # reset vector -> 0xC009; the word at 0xFFFE is SET R0 0xC0
    put16(img, 0xC009, 0xFF19)  # SET RA 0xFF
    put16(img, 0xC00B, 0xFE29)  # SET RB 0xFE   (pair = 0xFFFE)
    put16(img, 0xC00D, 0x021F)  # JMP ALWAYS RA RB -> 0xFFFE
    put16(img, 0xFFFC, 0xC020)  # fault vector
    put16(img, 0xC020, 0x421C)  # RSR CAUSE RA RB
    put16(img, 0xC022, 0x0020)  # HALT RB
    r: dict = run_until_halt(img)
    assert r["halt_code"] == 0x0A
