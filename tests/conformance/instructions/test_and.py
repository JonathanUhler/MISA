"""
Per-instruction conformance tests for AND.

Manual: `AND RD RS1 RS2` computes `RD = RS1 & RS2`. AND may update the ALU flags, so this suite
asserts only the documented register result.
"""


from harness import run, Reg


def test_and_masks_bits():
    """The result keeps only the bits set in both sources."""

    sim = run("""
        set ra 0xFF
        set rb 0x0F
        and rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0x0F


def test_and_with_disjoint_bits_is_zero():
    """ANDing values with no common bits yields zero."""

    sim = run("""
        set ra 0xF0
        set rb 0x0F
        and rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0x00
