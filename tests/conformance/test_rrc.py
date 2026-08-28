"""
RRC flag regression tests for finding A4.

RRC sets Z, N, and V from the 8-bit result and leaves C holding the shifted-out bit. The simulator
previously produced a spurious Z from an unrelated value. FLAGS packs as V, N, C, Z from bit 3 down
to bit 0.
"""


from harness import run_program, Reg, Csr


def test_rrc_sets_carry_and_clears_zero():
    """RRC of 0x03 with a clear carry in yields 0x01, with C set and Z clear."""

    sim = run_program("rrc_shift.S")
    assert sim.reg[Reg.RB] == 0x01
    assert sim.get_csr(Csr.FLAGS) == 0b0010


def test_rrc_shifts_carry_into_msb():
    """RRC of 0x00 with a set carry in yields 0x80, with N set and Z clear."""

    sim = run_program("rrc_carry_in.S")
    assert sim.reg[Reg.RE] == 0x80
    assert sim.get_csr(Csr.FLAGS) == 0b0100
