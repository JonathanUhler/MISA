"""
Per-instruction conformance tests for GOTO.

Manual: `GOTO IMM` jumps unconditionally to `IMM`, expanding to a `SET` pair into `RSCRATCH` and
`JMP ALWAYS RSCRATCH0 RSCRATCH1`.
"""


from harness import run, Reg


def test_goto_jumps_unconditionally():
    """GOTO transfers control to the label and skips the intervening instruction."""

    sim = run("""
        goto skip
        set ra 0xEE
        halt ra
    skip:
        set ra 0x2A
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
