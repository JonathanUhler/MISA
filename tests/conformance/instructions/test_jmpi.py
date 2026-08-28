"""
Per-instruction conformance tests for JMPI.

Manual: `JMPI CMP IMM` jumps to `IMM` when the comparison holds, expanding to a `SET` pair into
`RSCRATCH` and `JMP CMP RSCRATCH0 RSCRATCH1`.
"""


from harness import run, Reg


def test_jmpi_always_is_taken():
    """An unconditional immediate jump skips the fall-through instruction."""

    sim = run("""
        jmpi always skip
        set ra 0xEE
        halt ra
    skip:
        set ra 0x2A
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A


def test_jmpi_not_taken_falls_through():
    """A conditional immediate jump whose comparison is false continues in place."""

    sim = run("""
        clr
        jmpi equal skip
        set ra 0x2A
        halt ra
    skip:
        set ra 0xEE
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
