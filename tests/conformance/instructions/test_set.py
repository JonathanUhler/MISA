"""
Per-instruction conformance tests for SET.

Manual: `SET RD IMM` stores the 8-bit immediate into `RD`. SET must not update the ALU flags.
"""


from harness import run, Reg, Csr


def test_set_loads_immediate():
    """The immediate value lands in the destination register."""

    sim = run("""
        set ra 0x2A
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A


def test_set_does_not_change_flags():
    """A SET after a flag-setting add leaves the flags untouched."""

    sim = run("""
        set ra 0xFF
        set rb 0x01
        add rc ra rb
        set rd 0x2A
        halt rd
    """)
    assert sim.reg[Reg.RD] == 0x2A
    assert sim.get_csr(Csr.FLAGS) == 0b0011
