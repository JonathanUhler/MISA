"""
Per-instruction conformance tests for RRC2.

Manual: `RRC2 RD1 RD2 RS1 RS2` shifts the 17-bit value `{C, RS1, RS2}` right by one into
`{RD1, RD2}`, expanding to `RRC RD1 RS1` then `RRC RD2 RS2`. The old LSB of `RS2` is the new carry.
"""


from harness import run, Reg, Csr


def test_rrc2_shifts_16bit_pair():
    """The high byte shifts into the low byte, and the carry follows the last RRC."""

    sim = run("""
        clr
        set ra 0x03
        set rb 0x00
        rrc2 rc rd ra rb
        halt r0
    """)
    assert sim.reg[Reg.RC] == 0x01
    assert sim.reg[Reg.RD] == 0x80
    assert sim.get_csr(Csr.FLAGS) == 0b0100
