"""
Per-instruction conformance tests for AND2.

Manual: `AND2 RD1 RD2 RS1 RS2 RS3 RS4` computes the bitwise AND of `{RS1, RS2}` and `{RS3, RS4}`
into `{RD1, RD2}`, expanding to `AND RD2 RS2 RS4` then `AND RD1 RS1 RS3`.
"""


from harness import run, Reg


def test_and2_ands_16bit_pair():
    """Each byte of the result is the AND of the matching source bytes."""

    sim = run("""
        set ra 0xFF
        set rb 0xFF
        set rc 0x0F
        set rd 0xF0
        and2 re rf ra rb rc rd
        halt r0
    """)
    assert sim.reg[Reg.RE] == 0x0F
    assert sim.reg[Reg.RF] == 0xF0
