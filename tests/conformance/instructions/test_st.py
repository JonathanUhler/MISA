"""
Per-instruction conformance tests for ST.

Manual: `ST RD RS1 RS2` stores `Memory[RS1 << 8 | RS2] = RD`. The address is a fixed RAM location so
the written byte can be read back from simulator memory.
"""


from harness import run


def test_st_writes_register_to_memory():
    """Storing a register places its byte at the addressed RAM location."""

    sim = run("""
        set ra 0x02
        set rb 0x00
        set rc 0x2A
        st rc ra rb
        halt r0
    """)
    assert sim.mem[0x0200] == 0x2A
