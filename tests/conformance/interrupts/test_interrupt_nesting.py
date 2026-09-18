"""
Interrupt-in-syscall: an interrupt taken during a syscall context returns via RETI, then the
syscall returns via RETS. The two saved ring bits restore independently.
"""

from harness import Simulator, Csr


def _at(sim, address):
    sim.pc = address
    sim.in_reset = False


def test_interrupt_during_syscall_restores_rings_independently():
    sim = Simulator()
    # syscall handler at 0xFFFA -> 0xB000; interrupt handler at 0xFFF8 -> 0xC000
    sim.mem[0xFFFA] = 0x00
    sim.mem[0xFFFB] = 0xB0
    sim.mem[0xFFF8] = 0x00
    sim.mem[0xFFF9] = 0xC0
    # interrupt handler: immediate RETI (JMP ALWAYS RETIR)
    sim.mem[0xC000] = 0xF | (int(Csr.RETIR) << 4)
    sim.mem[0xC001] = 0x80
    # syscall handler: RETS (JMP ALWAYS RETSC)
    sim.mem[0xB000] = 0xF | (int(Csr.RETSC) << 4)
    sim.mem[0xB001] = 0x80
    # user code at 0x0200: SYSCALL RA (E=1), then a NOP to return to
    sim.mem[0x0200] = 0x0 | (0x1 << 4)   # SYSCALL RA
    sim.mem[0x0201] = 0x80               # E bit set
    sim.mem[0x0202] = 0x06
    sim.mem[0x0203] = 0x00               # NOP
    sim.set_csr(Csr.PRIVS, 0x0001)       # start in User Mode
    _at(sim, 0x0200)

    sim.step()                           # SYSCALL -> Machine Mode, in_syscall
    assert sim.in_syscall is True
    assert sim.get_csr(Csr.PRIVS) & 0b1 == 0
    sim.assert_interrupt(0x00)
    sim.step()                           # take interrupt (still Machine Mode)
    assert sim.in_interrupt is True
    sim.step()                           # RETI -> back in syscall handler, still in_syscall
    assert sim.in_interrupt is False
    assert sim.in_syscall is True
    sim.step()                           # RETS -> back to user, restore User Mode
    assert sim.in_syscall is False
    assert sim.get_csr(Csr.PRIVS) & 0b1 == 1
