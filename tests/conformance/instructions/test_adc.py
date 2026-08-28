"""
Per-instruction conformance tests for ADC.

Manual: `ADC RD RS1 RS2` computes `RD = RS1 + RS2 + C`. The carry is taken from the `C` field of the
`FLAGS` register left by the previous ALU operation.
"""


from harness import run, Reg, Csr


def test_adc_without_carry():
    """With the carry clear, ADC behaves like an ordinary add."""

    sim = run("""
        clr
        set rc 0x10
        set rd 0x20
        adc re rc rd
        halt re
    """)
    assert sim.reg[Reg.RE] == 0x30
    assert sim.get_csr(Csr.FLAGS) == 0b0000


def test_adc_with_carry():
    """A carry left by a previous add is folded into the sum."""

    sim = run("""
        set ra 0xFF
        set rb 0x01
        add r0 ra rb
        set rc 0x10
        set rd 0x20
        adc re rc rd
        halt re
    """)
    assert sim.reg[Reg.RE] == 0x31
