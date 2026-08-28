"""
Per-instruction conformance tests for JALI.

Manual: `JALI CMP IMM` links and jumps to `IMM` when the comparison holds, expanding to a `SET` pair
into `RSCRATCH` and `JAL CMP RSCRATCH0 RSCRATCH1`.
"""


from harness import run, Reg


def test_jali_links_and_returns():
    """A taken JALI records the return address, so a later RET lands after the JALI."""

    sim = run("""
        jali always subr
        set ra 0x2A
        halt ra
    subr:
        ret
    """)
    assert sim.reg[Reg.RA] == 0x2A


def test_jali_not_taken_does_not_branch():
    """A JALI whose comparison is false neither branches nor links."""

    sim = run("""
        clr
        jali equal subr
        set ra 0x2A
        halt ra
    subr:
        set ra 0xEE
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
