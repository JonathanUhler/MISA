"""
Per-instruction conformance tests for HALT.

Manual: `HALT RS` sets `CAUSE = RS << 8 | 0x01` and stops the processor. The `CAUSE` reason is
Instruction usage, the type is Halt, and the extended status is the value of `RS`.
"""


from harness import run, Csr, CauseReason, CauseTypeInstruction


def test_halt_sets_cause_and_stops():
    """Halting records the halt cause with the operand as the extended status and enters reset."""

    sim = run("""
        set ra 0x2A
        halt ra
    """)
    assert sim.in_reset
    cause: int = sim.get_csr(Csr.CAUSE)
    assert cause & 0b111 == CauseReason.INSTRUCTION
    assert (cause >> 3) & 0b111 == CauseTypeInstruction.HALT
    assert cause >> 8 == 0x2A
