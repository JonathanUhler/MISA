"""
Per-instruction conformance tests for RRC.

Manual: `RRC RD RS` shifts the 9-bit value `{C, RS}` right by one, storing the result in `RD` and
the old LSB of `RS` into `C`. `Z` and `N` come from the 8-bit result and `V` is cleared.

These cases also guard finding A4, where the simulator previously produced a spurious `Z` from an
unrelated value rather than from the shifted result.
"""


from harness import run, Reg, Csr


def test_rrc_shifts_right_and_captures_lsb():
    """A right rotate with the carry clear halves the value and captures the shifted-out bit."""

    sim = run("""
        clr
        set ra 0x03
        rrc rb ra
        halt rb
    """)
    assert sim.reg[Reg.RB] == 0x01
    assert sim.get_csr(Csr.FLAGS) == 0b0010


def test_rrc_rotates_carry_into_msb():
    """A set carry is rotated into the most significant bit, which sets the negative flag."""

    sim = run("""
        set ra 1
        set rb 1
        sub r0 ra rb
        set rc 0x00
        rrc rd rc
        halt rd
    """)
    assert sim.reg[Reg.RD] == 0x80
    assert sim.get_csr(Csr.FLAGS) == 0b0100
