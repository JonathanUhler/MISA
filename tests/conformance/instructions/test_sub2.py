"""
Per-instruction conformance tests for SUB2.

Manual: `SUB2 RD1 RD2 RS1 RS2 RS3 RS4` subtracts `{RS3, RS4}` from `{RS1, RS2}` into `{RD1, RD2}`,
expanding to `SUB RD2 RS2 RS4` then `SBB RD1 RS1 RS3`.
"""


from harness import run, Reg


def test_sub2_subtracts_16bit_pair_with_borrow_between_bytes():
    """The low-byte borrow propagates into the high byte of the 16-bit difference."""

    sim = run("""
        set ra 0x01
        set rb 0x00
        set rc 0x00
        set rd 0x01
        sub2 re rf ra rb rc rd
        halt r0
    """)
    assert sim.reg[Reg.RE] == 0x00
    assert sim.reg[Reg.RF] == 0xFF
