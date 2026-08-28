"""
Per-instruction conformance tests for POP.

Manual: `POP RD` increments `SADDR` by one byte and then loads the byte at the new stack address
into `RD`. A known byte is stored above the seeded stack pointer first.
"""


from harness import run, Reg, Csr


def test_pop_increments_saddr_then_reads():
    """A pop moves the stack pointer up by one and loads the byte it then points at."""

    sim = run("""
        set ra 0x01
        set rb 0xFF
        set rc 0x2A
        st rc ra rb
        set ra 0x01
        set rb 0xFE
        wsr saddr ra rb
        pop rd
        halt rd
    """)
    assert sim.reg[Reg.RD] == 0x2A
    assert sim.get_csr(Csr.SADDR) == 0x01FF
