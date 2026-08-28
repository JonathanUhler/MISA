"""
Per-instruction conformance tests for PUSH.

Manual: `PUSH RS` writes `RS` to the stack at `SADDR` and then decrements `SADDR` by one byte. The
stack pointer is seeded to a conventional value first.
"""


from harness import run, Csr


def test_push_writes_then_decrements_saddr():
    """A push stores the byte at SADDR and moves the stack pointer down by one."""

    sim = run("""
        set ra 0x01
        set rb 0xFF
        wsr saddr ra rb
        set rc 0x2A
        push rc
        halt r0
    """)
    assert sim.mem[0x01FF] == 0x2A
    assert sim.get_csr(Csr.SADDR) == 0x01FE
