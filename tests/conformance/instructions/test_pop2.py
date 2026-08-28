"""
Per-instruction conformance tests for POP2.

Manual: `POP2 RD1 RD2` pops a 16-bit value off the stack, expanding to `POP RD1` then `POP RD2`.
This round-trips a pair pushed with PUSH2 to confirm the byte order is symmetric.
"""


from harness import run, Reg, Csr


def test_pop2_recovers_pushed_pair():
    """Popping a pair restores the value pushed by PUSH2 and returns SADDR to its start."""

    sim = run("""
        set ra 0x01
        set rb 0xFF
        wsr saddr ra rb
        set ra 0x12
        set rb 0x34
        push2 ra rb
        pop2 rc rd
        halt r0
    """)
    assert sim.reg[Reg.RC] == 0x12
    assert sim.reg[Reg.RD] == 0x34
    assert sim.get_csr(Csr.SADDR) == 0x01FF
