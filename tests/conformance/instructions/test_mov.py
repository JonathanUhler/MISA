"""
Per-instruction conformance tests for MOV.

Manual: `MOV RD RS1` copies `RS1` into `RD`, expanding to `OR RD RS1 R0`.
"""


from harness import run, Reg


def test_mov_copies_register():
    """The destination receives a copy of the source register."""

    sim = run("""
        set ra 0x2A
        mov rb ra
        halt rb
    """)
    assert sim.reg[Reg.RB] == 0x2A
