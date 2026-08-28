"""
Per-instruction conformance tests for RSR.

Manual: `RSR CSR RS1 RS2` reads `RS1 = CSR >> 8` and `RS2 = CSR & 0xFF`. A known value is written to
`SADDR` first and then read back into a register pair.
"""


from harness import run, Reg


def test_rsr_reads_csr_into_register_pair():
    """The register pair receives the high and low bytes of the control register."""

    sim = run("""
        set ra 0x12
        set rb 0x34
        wsr saddr ra rb
        rsr saddr rc rd
        halt r0
    """)
    assert sim.reg[Reg.RC] == 0x12
    assert sim.reg[Reg.RD] == 0x34
