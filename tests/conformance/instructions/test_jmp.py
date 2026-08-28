"""
Per-instruction conformance tests for JMP.

Manual: `JMP CMP RS1 RS2` sets `PC = RS1 << 8 | RS2` when the comparison holds. A taken branch
skips a sentinel, and a branch that is not taken falls through to it.
"""


from harness import run, Reg


def test_jmp_always_is_taken():
    """An unconditional jump transfers control and skips the fall-through instruction."""

    sim = run("""
        set2 rc rd taken
        jmp always rc rd
        set ra 0xEE
        halt ra
    taken:
        set ra 0x2A
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A


def test_jmp_not_taken_falls_through():
    """A conditional jump whose comparison is false continues with the next instruction."""

    sim = run("""
        clr
        set2 rc rd taken
        jmp equal rc rd
        set ra 0x2A
        halt ra
    taken:
        set ra 0xEE
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
