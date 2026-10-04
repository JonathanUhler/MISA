"""
Per-instruction conformance tests for LD.

Manual: `LD RD RSB IMM` loads `RD = Memory[{RSB, RSB + 1} + IMM]`, naming the high base register.
The base pair is built with SET2 from a data label, so the load reads a known seeded byte.
"""


from harness import run, Reg


def test_ld_reads_memory_into_register():
    """Loading from a data address places the stored byte into the destination."""

    sim = run(
        """
        set2 ra rb value
        ld rc ra 0
        halt rc
        """,
        data="""
        value:
            .word 0x2A
        """,
    )
    assert sim.reg[Reg.RC] == 0x2A
