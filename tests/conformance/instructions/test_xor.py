"""
Per-instruction conformance tests for XOR.

Manual: `XOR RD RS1 RS2` computes `RD = RS1 ^ RS2`. XOR may update the ALU flags, so this suite
asserts only the documented register result.
"""


from harness import run, Reg


def test_xor_computes_bitwise_xor():
    """The result holds the bitwise exclusive OR of the two sources."""

    sim = run("""
        set ra 0xFF
        set rb 0x0F
        xor rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0xF0


def test_xor_with_self_is_zero():
    """XORing a value with itself clears the destination."""

    sim = run("""
        set ra 0x5A
        xor rc ra ra
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0x00
