"""
Core interrupt control flow: vectoring, shadow save/restore, masking, and privilege transition.
`assert_interrupt` is the test hook that raises the pin and enqueues an entry.
"""

from harness import Simulator, Reg, Csr


def _at(sim: Simulator, address: int) -> None:
    sim.pc = address
    sim.in_reset = False


def test_interrupt_vectors_and_saves_return_address():
    sim = Simulator()
    handler = 0xC000
    sim.mem[0xFFF8] = handler & 0xFF
    sim.mem[0xFFF9] = handler >> 8
    sim.mem[0x0200] = 0x06  # a NOP (OR R0 R0 R0) that must NOT execute before vectoring
    sim.mem[0x0201] = 0x00
    _at(sim, 0x0200)
    cause_before = sim.get_csr(Csr.CAUSE)

    sim.assert_interrupt(0x03, 0x1234)
    sim.step()

    assert sim.pc == handler
    assert sim.get_csr(Csr.RETIR) == 0x0200          # resumes at the not-yet-run instruction
    assert sim.in_interrupt is True
    assert sim.get_csr(Csr.CAUSE) == cause_before     # interrupts do not touch CAUSE
    assert sim.get_csr(Csr.PRIVS) & 0b1 == 0          # Machine Mode


def test_reti_restores_shadowed_rscratch_and_flags():
    sim = Simulator()
    handler = 0xC000
    sim.mem[0xFFF8] = handler & 0xFF
    sim.mem[0xFFF9] = handler >> 8
    # handler at 0xC000: clobber RSCRATCH0/FLAGS, then RETI (JMP ALWAYS RETIR).
    sim.mem[0xC000] = 0x9 | (int(Reg.RSCRATCH0) << 4)  # SET RSCRATCH0 0xEE
    sim.mem[0xC001] = 0xEE
    sim.mem[0xC002] = 0xF | (int(Csr.RETIR) << 4)      # JMP ALWAYS RETIR
    sim.mem[0xC003] = 0x80
    sim.mem[0x0200] = 0x06  # NOP to return to
    sim.mem[0x0201] = 0x00
    sim.set_reg(Reg.RSCRATCH0, 0x11)
    sim.set_csr(Csr.FLAGS, 0b0101)
    _at(sim, 0x0200)

    sim.assert_interrupt(0x00)
    sim.step()   # vector
    sim.step()   # SET RSCRATCH0 0xEE
    sim.step()   # RETI

    assert sim.in_interrupt is False
    assert sim.reg[Reg.RSCRATCH0] == 0x11             # shadow restored
    assert sim.get_csr(Csr.FLAGS) == 0b0101
    assert sim.pc == 0x0200


def test_second_interrupt_is_masked_until_reti():
    sim = Simulator()
    handler = 0xC000
    sim.mem[0xFFF8] = handler & 0xFF
    sim.mem[0xFFF9] = handler >> 8
    sim.mem[0xC000] = 0xF | (int(Csr.RETIR) << 4)     # immediate RETI
    sim.mem[0xC001] = 0x80
    sim.mem[0x0200] = 0x06
    sim.mem[0x0201] = 0x00
    _at(sim, 0x0200)

    sim.assert_interrupt(0x01)
    sim.step()                       # take first
    assert sim.in_interrupt is True
    sim.assert_interrupt(0x02)       # arrives while masked
    sim.step()                       # RETI, leaves interrupt context
    assert sim.in_interrupt is False
    sim.step()                       # now the second is taken
    assert sim.in_interrupt is True


def test_interrupt_from_user_mode_enters_machine_and_reti_restores_user():
    sim = Simulator()
    handler = 0xC000
    sim.mem[0xFFF8] = handler & 0xFF
    sim.mem[0xFFF9] = handler >> 8
    sim.mem[0xC000] = 0xF | (int(Csr.RETIR) << 4)
    sim.mem[0xC001] = 0x80
    sim.mem[0x0200] = 0x06
    sim.mem[0x0201] = 0x00
    sim.set_csr(Csr.PRIVS, 0x0001)   # User Mode
    _at(sim, 0x0200)

    sim.assert_interrupt(0x00)
    sim.step()                       # vector -> Machine Mode
    assert sim.get_csr(Csr.PRIVS) & 0b1 == 0
    sim.step()                       # RETI -> restore User Mode
    assert sim.get_csr(Csr.PRIVS) & 0b1 == 1
