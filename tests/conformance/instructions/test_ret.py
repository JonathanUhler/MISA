"""
Per-instruction conformance tests for RET.

Manual: `RET` expands to `RSR RADDR RSCRATCH`, `JMP ALWAYS RSCRATCH`. It returns to the address held
in `RADDR`, which is seeded here with a WSR.
"""


from harness import run, Reg


def test_ret_jumps_to_link_register():
    """RET transfers control to the address in RADDR and skips the following instruction."""

    sim = run("""
        set2 rc rd target
        wsr raddr rc rd
        ret
        set ra 0xEE
        halt ra
    target:
        set ra 0x2A
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
