"""
Per-instruction conformance tests for CLR.

Manual: `CLR` clears the `FLAGS` register, expanding to `WSR FLAGS R0 R0`.
"""


from harness import run, Csr


def test_clr_lowers_all_flags():
    """CLR resets every flag raised by a previous ALU operation."""

    sim = run("""
        set ra 0xFF
        set rb 0x01
        add rc ra rb
        clr
        halt r0
    """)
    assert sim.get_csr(Csr.FLAGS) == 0b0000
