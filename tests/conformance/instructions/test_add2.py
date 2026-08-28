"""
Per-instruction conformance tests for ADD2.

Manual: `ADD2 RD1 RD2 RS1 RS2 RS3 RS4` adds the 16-bit values `{RS1, RS2}` and `{RS3, RS4}` into
`{RD1, RD2}`, expanding to `ADD RD2 RS2 RS4` then `ADC RD1 RS1 RS3`.
"""


from harness import run, Reg


def test_add2_adds_16bit_pair_with_carry_between_bytes():
    """The low-byte carry propagates into the high byte of the 16-bit sum."""

    sim = run("""
        set ra 0x00
        set rb 0xFF
        set rc 0x00
        set rd 0x01
        add2 re rf ra rb rc rd
        halt r0
    """)
    assert sim.reg[Reg.RE] == 0x01
    assert sim.reg[Reg.RF] == 0x00
