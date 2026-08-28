"""
Baseline conformance tests for the MISA reference simulator.

These programs already pass on the current simulator and lock in correct behavior for basic
arithmetic, memory access, and subroutine calls. Later sessions add regression tests for specific
fixes alongside these baselines.
"""


from harness import run_program, Reg, Csr


def test_arithmetic_add():
    """Adds two immediates and halts with the sum in RC."""

    sim = run_program("arithmetic.S")
    assert sim.reg[Reg.RC] == 8
    assert sim.get_csr(Csr.CAUSE) == (8 << 8) | 0x01


def test_load_store_round_trip():
    """Stores a byte to RAM, loads it back, and halts with it in RD."""

    sim = run_program("loadstore.S")
    assert sim.reg[Reg.RD] == 0x2A
    assert sim.mem[0x0200] == 0x2A


def test_call_and_return():
    """Calls a subroutine that sets RA, returns, and halts with RA."""

    sim = run_program("callret.S")
    assert sim.reg[Reg.RA] == 0x63
    assert sim.get_csr(Csr.CAUSE) == (0x63 << 8) | 0x01
