"""
Control-event faults (design §1.4). These drive the simulator directly and assert the CAUSE reason
and type after a single faulting step. A zero fault vector sends control to 0x0000, but the fault's
CAUSE is read immediately after the step.
"""

from harness import Simulator, Csr, Reg, CauseReason, CauseTypeControl


def _prime_fault_vector(sim: Simulator) -> None:
    sim.mem[0xFFFC] = 0x00
    sim.mem[0xFFFD] = 0x00


def test_conditional_context_return_faults():
    """JMP EQUAL RETSC (non-ALWAYS to a context CSR) faults."""
    sim = Simulator()
    _prime_fault_vector(sim)
    sim.mem[0x0000] = 0xF | (int(Csr.RETSC) << 4)   # 0x8F
    sim.mem[0x0001] = (int(1) << 4) | 0x80          # CMP=EQUAL, selector=1
    sim.pc = 0x0000
    sim.in_reset = False
    sim.step()
    cause = sim.get_csr(Csr.CAUSE)
    assert cause & 0b111 == CauseReason.CONTROL
    assert (cause >> 3) & 0b111 == CauseTypeControl.CONDITIONAL_RETURN


def test_improper_return_context_faults_outside_syscall():
    """RETS (JMP ALWAYS RETSC) outside a syscall context faults."""
    sim = Simulator()
    _prime_fault_vector(sim)
    sim.mem[0x0000] = 0xF | (int(Csr.RETSC) << 4)
    sim.mem[0x0001] = 0x80                           # CMP=ALWAYS, selector=1
    sim.pc = 0x0000
    sim.in_reset = False
    sim.step()
    cause = sim.get_csr(Csr.CAUSE)
    assert cause & 0b111 == CauseReason.CONTROL
    assert (cause >> 3) & 0b111 == CauseTypeControl.IMPROPER_RETURN


def test_improper_return_context_faults_reti_outside_interrupt():
    """RETI (JMP ALWAYS RETIR) outside an interrupt context faults."""
    sim = Simulator()
    _prime_fault_vector(sim)
    sim.mem[0x0000] = 0xF | (int(Csr.RETIR) << 4)   # 0x9F
    sim.mem[0x0001] = 0x80
    sim.pc = 0x0000
    sim.in_reset = False
    sim.step()
    cause = sim.get_csr(Csr.CAUSE)
    assert cause & 0b111 == CauseReason.CONTROL
    assert (cause >> 3) & 0b111 == CauseTypeControl.IMPROPER_RETURN


def test_syscall_in_syscall_context_faults():
    """A SYSCALL while already in a syscall context faults as improper system call."""
    sim = Simulator()
    _prime_fault_vector(sim)
    sim.in_syscall = True
    sim.mem[0x0000] = 0x0 | (int(Reg.RA) << 4)      # SYSCALL RA
    sim.mem[0x0001] = 0x80                           # E=1
    sim.pc = 0x0000
    sim.in_reset = False
    sim.step()
    cause = sim.get_csr(Csr.CAUSE)
    assert cause & 0b111 == CauseReason.CONTROL
    assert (cause >> 3) & 0b111 == CauseTypeControl.IMPROPER_SYSCALL
