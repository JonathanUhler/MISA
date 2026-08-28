"""
Per-instruction conformance tests for WSR.

Manual: `WSR RS1 RS2 CSR` writes `CSR = RS1 << 8 | RS2`. From the reset state the simulator is
privileged, so a write to `SADDR` is permitted.
"""


from harness import run, Csr


def test_wsr_writes_register_pair_to_csr():
    """The control register takes the 16-bit value formed by the register pair."""

    sim = run("""
        set ra 0x12
        set rb 0x34
        wsr saddr ra rb
        halt r0
    """)
    assert sim.get_csr(Csr.SADDR) == 0x1234
