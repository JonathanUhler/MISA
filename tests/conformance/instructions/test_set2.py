"""
Per-instruction conformance tests for SET2.

Manual: `SET2 RS1 RS2 IMM` loads the high and low halves of a 16-bit immediate into `RS1` and `RS2`,
expanding to `SET RS1 (IMM >> 8)` then `SET RS2 (IMM & 0xFF)`.
"""


from harness import run, Reg


def test_set2_loads_16bit_immediate():
    """The high byte lands in the first register and the low byte in the second."""

    sim = run("""
        set2 ra rb 0x1234
        halt r0
    """)
    assert sim.reg[Reg.RA] == 0x12
    assert sim.reg[Reg.RB] == 0x34
