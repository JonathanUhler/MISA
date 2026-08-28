"""
Per-instruction conformance tests for SUB.

Manual: `SUB RD RS1 RS2` computes `RD = RS1 - RS2` and updates the ALU flags. The architecture
uses a carry convention, so `C` is set when the subtraction does not borrow.
"""


from harness import run, Reg, Csr


def test_sub_basic():
    """A subtraction without borrow stores the difference and sets the carry flag."""

    sim = run("""
        set ra 8
        set rb 3
        sub rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 5
    assert sim.get_csr(Csr.FLAGS) == 0b0010


def test_sub_equal_sets_zero_and_carry():
    """Subtracting equal values yields zero with the zero and carry flags set."""

    sim = run("""
        set ra 5
        set rb 5
        sub rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0
    assert sim.get_csr(Csr.FLAGS) == 0b0011


def test_sub_borrow_clears_carry():
    """A subtraction that borrows wraps the result and clears the carry flag."""

    sim = run("""
        set ra 3
        set rb 8
        sub rc ra rb
        halt rc
    """)
    assert sim.reg[Reg.RC] == 0xFB
    assert sim.get_csr(Csr.FLAGS) == 0b0100
