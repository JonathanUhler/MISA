"""
Per-instruction conformance tests for PUSH2.

Manual: `PUSH2 RS1 RS2` pushes a 16-bit value onto the stack, expanding to `PUSH RS2` then `PUSH
RS1`. The low byte is written first, so the high byte ends up at the lower address.
"""


from harness import run, Csr


def test_push2_pushes_pair_high_byte_last():
    """A pair push writes the low byte then the high byte and drops SADDR by two."""

    sim = run("""
        set ra 0x01
        set rb 0xFF
        wsr saddr ra rb
        set ra 0x12
        set rb 0x34
        push2 ra rb
        halt r0
    """)
    assert sim.mem[0x01FF] == 0x34
    assert sim.mem[0x01FE] == 0x12
    assert sim.get_csr(Csr.SADDR) == 0x01FD
