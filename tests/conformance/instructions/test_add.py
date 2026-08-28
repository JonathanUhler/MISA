"""
Per-instruction conformance tests for ADD.

Manual: `ADD RD RS1 RS2` computes `RD = RS1 + RS2` and updates the ALU flags from the 8-bit result.
FLAGS packs as V, N, C, Z from bit 3 down to bit 0.
"""


from harness import run, Reg, Csr


def test_add_basic():
    """Adding two small values stores the sum and leaves every flag clear."""

    sim = run("""
        set ra 5
        set rb 3
        add rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 8
    assert sim.get_csr(Csr.FLAGS) == 0b0000


def test_add_carry_out_and_zero():
    """A sum that wraps the byte to zero sets the carry and zero flags."""

    sim = run("""
        set ra 0xFF
        set rb 0x01
        add rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0x00
    assert sim.get_csr(Csr.FLAGS) == 0b0011


def test_add_signed_overflow():
    """A sum that crosses the signed boundary sets the negative and overflow flags."""

    sim = run("""
        set ra 0x7F
        set rb 0x01
        add rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0x80
    assert sim.get_csr(Csr.FLAGS) == 0b1100
