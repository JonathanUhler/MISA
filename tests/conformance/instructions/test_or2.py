"""
Per-instruction conformance tests for OR2.

Manual: `OR2 RD1 RD2 RS1 RS2 RS3 RS4` computes the bitwise OR of `{RS1, RS2}` and `{RS3, RS4}` into
`{RD1, RD2}`, expanding to `OR RD2 RS2 RS4` then `OR RD1 RS1 RS3`.
"""


from harness import run, Reg


def test_or2_ors_16bit_pair():
    """Each byte of the result is the OR of the matching source bytes."""

    sim = run("""
        set ra 0xF0
        set rb 0x0F
        set rc 0x0F
        set rd 0xF0
        or2 re rf ra rb rc rd
        halt r0
    """)
    assert sim.reg[Reg.RE] == 0xFF
    assert sim.reg[Reg.RF] == 0xFF
