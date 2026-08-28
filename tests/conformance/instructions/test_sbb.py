"""
Per-instruction conformance tests for SBB.

Manual: `SBB RD RS1 RS2` computes `RD = RS1 + ~RS2 + C`. Because `C` is the adder carry out, this
equals `RS1 - RS2 - (1 - C)`, so chaining SUB then SBB subtracts a multi-byte value with the borrow
carried through `C`.
"""


from harness import run, Reg


def test_sbb_with_carry_set_subtracts_without_extra_borrow():
    """With C set from a prior no-borrow subtraction, SBB subtracts the operands directly."""

    sim = run("""
        set ra 5
        set rb 5
        sub r0 ra rb
        set rc 0x20
        set rd 0x05
        sbb re rc rd
        halt re
    """)
    assert sim.reg[Reg.RE] == 0x1B


def test_sbb_with_carry_clear_subtracts_extra_borrow():
    """With C cleared by a prior borrow, SBB subtracts an additional one."""

    sim = run("""
        set ra 3
        set rb 8
        sub r0 ra rb
        set rc 0x20
        set rd 0x05
        sbb re rc rd
        halt re
    """)
    assert sim.reg[Reg.RE] == 0x1A
