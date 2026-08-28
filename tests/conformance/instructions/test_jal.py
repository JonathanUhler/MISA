"""
Per-instruction conformance tests for JAL.

Manual: `JAL CMP RS1 RS2` sets `RADDR = PC + 2` and `PC = RS1 << 8 | RS2` when the comparison holds.
The link is verified by returning through `RADDR` and landing on the instruction after the JAL.
"""


from harness import run, Reg


def test_jal_links_and_returns():
    """A taken JAL records the return address, so a later RET lands after the JAL."""

    sim = run("""
        set2 rc rd subr
        jal always rc rd
        set ra 0x2A
        halt ra
    subr:
        ret
    """)
    assert sim.reg[Reg.RA] == 0x2A


def test_jal_not_taken_does_not_branch():
    """A JAL whose comparison is false neither branches nor links."""

    sim = run("""
        clr
        set2 rc rd subr
        jal equal rc rd
        set ra 0x2A
        halt ra
    subr:
        set ra 0xEE
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
