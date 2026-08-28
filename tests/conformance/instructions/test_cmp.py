"""
Per-instruction conformance tests for CMP.

Manual: `CMP RS1 RS2` sets the ALU flags for the comparison `RS1 ? RS2`, expanding to `SUB R0 RS1
RS2`. The result is discarded into `R0` and only the flags matter.
"""


from harness import run, Reg, Csr


def test_cmp_equal_sets_zero_and_carry():
    """Comparing equal values sets the zero and carry flags."""

    sim = run("""
        set ra 5
        set rb 5
        cmp ra rb
        halt r0
    """)
    assert sim.get_csr(Csr.FLAGS) == 0b0011
    assert sim.reg[Reg.R0] == 0


def test_cmp_greater_sets_carry_only():
    """Comparing a larger value against a smaller one sets only the carry flag."""

    sim = run("""
        set ra 5
        set rb 3
        cmp ra rb
        halt r0
    """)
    assert sim.get_csr(Csr.FLAGS) == 0b0010
