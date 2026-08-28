"""
Per-instruction conformance tests for NOP.

Manual: `NOP` does nothing, expanding to `OR R0 R0 R0`.
"""


from harness import run, Reg


def test_nop_leaves_registers_unchanged():
    """A NOP between two instructions does not disturb register state."""

    sim = run("""
        set ra 0x2A
        nop
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
