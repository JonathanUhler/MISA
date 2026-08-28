"""
PC overflow regression tests for finding A2.

These tests drive the simulator directly rather than through the assembly pipeline. The overflow
point sits at address 0xFFFE, which is also the reset vector, so a normal assembled program cannot
reach it without executing the vector table. Building the state by hand keeps the intent clear.

The simulator now masks memory addresses to 16 bits and faults when the program counter leaves the
16-bit range at the end of a step. A taken jump overwrites the program counter first, so it does not
fault.
"""


from harness import Simulator, Reg, Csr, CauseReason, CauseTypeMemory


def _start_at(sim: Simulator, address: int) -> None:
    """Places the simulator at an address without going through reset."""

    sim.pc = address
    sim.in_reset = False


def test_pc_overflow_faults():
    """Running off the top of memory faults to the fault vector with a PC overflow cause."""

    sim = Simulator()
    handler: int = 0x1234
    sim.mem[0xFFFC] = handler & 0xFF
    sim.mem[0xFFFD] = handler >> 8

    # An OR R0 R0 R0 (NOP) at the very top of memory.
    sim.mem[0xFFFE] = 0x06
    sim.mem[0xFFFF] = 0x00
    _start_at(sim, 0xFFFE)

    sim.step()

    assert sim.pc == handler
    cause: int = sim.get_csr(Csr.CAUSE)
    assert cause & 0b111 == CauseReason.MEMORY
    assert (cause >> 3) & 0b111 == CauseTypeMemory.PC


def test_pc_overflow_rescued_by_jump():
    """A taken jump from the top of memory redirects the program counter and does not fault."""

    sim = Simulator()

    # A JMP ALWAYS RA RB at the top of memory, targeting 0x1234.
    sim.mem[0xFFFE] = 0xF | (int(Reg.RA) << 4)
    sim.mem[0xFFFF] = int(Reg.RB)
    sim.set_reg(Reg.RA, 0x12)
    sim.set_reg(Reg.RB, 0x34)
    _start_at(sim, 0xFFFE)
    cause_before: int = sim.get_csr(Csr.CAUSE)

    sim.step()

    assert sim.pc == 0x1234
    assert sim.get_csr(Csr.CAUSE) == cause_before
