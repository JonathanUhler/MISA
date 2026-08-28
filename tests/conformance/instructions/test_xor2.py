"""
Per-instruction conformance tests for XOR2.

Manual: `XOR2 RD1 RD2 RS1 RS2 RS3 RS4` computes the bitwise XOR of `{RS1, RS2}` and `{RS3, RS4}`
into `{RD1, RD2}`, expanding to `XOR RD2 RS2 RS4` then `XOR RD1 RS1 RS3`.
"""


from harness import run, Reg


def test_xor2_xors_16bit_pair():
    """Each byte of the result is the XOR of the matching source bytes."""

    sim = run("""
        set ra 0xFF
        set rb 0x0F
        set rc 0x0F
        set rd 0xFF
        xor2 re rf ra rb rc rd
        halt r0
    """)
    assert sim.reg[Reg.RE] == 0xF0
    assert sim.reg[Reg.RF] == 0xF0
