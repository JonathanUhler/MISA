"""
Interrupt queue handshake. The queue lives at IRQ_BASE. The handler pops with the four-phase
protocol: write pop=1, wait ack=1, write pop=0, wait ack=0. One hardware tick runs per step.
"""

from harness import Simulator, Csr
from simulator import IRQ_BASE


def _idle_step(sim: Simulator) -> None:
    """Steps the queue hardware once without executing program code (NOP at PC)."""
    sim.mem[sim.pc] = 0x06
    sim.mem[sim.pc + 1] = 0x00
    sim.step()


def test_length_and_head_reflect_enqueued_entries():
    sim = Simulator()
    sim.pc = 0x0200
    sim.in_reset = False
    sim.assert_interrupt(0x07, 0xBEEF)
    sim.assert_interrupt(0x09, 0x1234)
    # Take the interrupt so we are in the handler context, then let one hardware tick refresh fields.
    sim.mem[0xFFF8] = 0x00
    sim.mem[0xFFF9] = 0xC0
    sim.mem[0xC000] = 0x06
    sim.mem[0xC001] = 0x00
    sim.step()          # vector to 0xC000
    _idle_step(sim)     # one queue tick refreshes the memory-mapped fields
    assert sim.read_mem(IRQ_BASE + 0) == 2          # length
    assert sim.read_mem(IRQ_BASE + 3) == 0x07       # head number
    assert sim.read_mem(IRQ_BASE + 4) == 0xEF       # argptr low
    assert sim.read_mem(IRQ_BASE + 5) == 0xBE       # argptr high


def test_four_phase_pop_advances_the_head():
    sim = Simulator()
    sim.pc = 0xC000
    sim.in_reset = False
    sim.in_interrupt = True
    sim.assert_interrupt(0x07, 0xBEEF)
    sim.assert_interrupt(0x09, 0x1234)

    _idle_step(sim)                       # publish head
    assert sim.read_mem(IRQ_BASE + 3) == 0x07
    sim.write_mem(IRQ_BASE + 1, 0x01)     # pop = 1 (request)
    _idle_step(sim)                       # rising edge: pop head, ack = 1
    assert sim.read_mem(IRQ_BASE + 2) == 1
    assert sim.read_mem(IRQ_BASE + 0) == 1            # length decremented
    assert sim.read_mem(IRQ_BASE + 3) == 0x09        # head advanced
    sim.write_mem(IRQ_BASE + 1, 0x00)     # pop = 0 (withdraw)
    _idle_step(sim)                       # falling edge: ack = 0
    assert sim.read_mem(IRQ_BASE + 2) == 0
