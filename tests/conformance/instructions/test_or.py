"""
Per-instruction conformance tests for OR.

Manual: `OR RD RS1 RS2` computes `RD = RS1 | RS2`. OR may update the ALU flags, so this suite
asserts only the documented register result.
"""


from harness import run, Reg


def test_or_computes_bitwise_or():
    """The result holds the bitwise OR of the two sources."""

    sim = run("""
        set ra 0xF0
        set rb 0x0F
        or rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0xFF


def test_or_with_zero_is_identity():
    """OR with zero copies the other operand."""

    sim = run("""
        set ra 0x5A
        or rc ra r0
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0x5A
