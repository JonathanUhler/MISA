"""
Per-instruction conformance tests for MOV2.

Manual: `MOV2 RD1 RD2 RS1 RS2` copies the 16-bit value `{RS1, RS2}` into `{RD1, RD2}`, expanding to
`MOV RD2 RS2` then `MOV RD1 RS1`.
"""


from harness import run, Reg


def test_mov2_copies_register_pair():
    """Both halves of the source pair are copied into the destination pair."""

    sim = run("""
        set ra 0x12
        set rb 0x34
        mov2 rc rd ra rb
        halt r0
    """)
    assert sim.reg[Reg.RC] == 0x12
    assert sim.reg[Reg.RD] == 0x34
