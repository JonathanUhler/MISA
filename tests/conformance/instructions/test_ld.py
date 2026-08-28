"""
Per-instruction conformance tests for LD.

Manual: `LD RD RS1 RS2` loads `RD = Memory[RS1 << 8 | RS2]`. The address pair is built with SET2
from a data label, so the load reads a known seeded byte.
"""


from harness import run, Reg


def test_ld_reads_memory_into_register():
    """Loading from a data address places the stored byte into the destination."""

    sim = run(
        """
        set2 ra rb value
        ld rc ra rb
        halt rc
        """,
        data="""
        value:
            .word 0x2A
        """,
    )
    assert sim.reg[Reg.RC] == 0x2A
