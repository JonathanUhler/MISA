"""
Per-instruction conformance tests for CALL.

Manual: `CALL IMM` expands to `SET RSCRATCH0 (IMM >> 8)`, `SET RSCRATCH1 (IMM & 0xFF)`,
`JAL ALWAYS RSCRATCH0 RSCRATCH1`. It calls a subroutine and links through `RADDR`.
"""


from harness import run, Reg


def test_call_invokes_and_returns():
    """A call enters the subroutine and returns to the instruction after the call."""

    sim = run("""
        call subr
        set ra 0x2A
        halt ra
    subr:
        ret
    """)
    assert sim.reg[Reg.RA] == 0x2A
