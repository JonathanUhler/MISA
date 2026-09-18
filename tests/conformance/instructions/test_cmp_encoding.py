"""
Encoding tests for the dense 3-bit comparison field (design §1.1).

These drive the simulator directly with hand-assembled JMP words so they do not depend on the
assembler. A GP-register JMP is `0xF | RS1<<4` in byte 0 and `RS2 | CMP<<4` in byte 1, with the
selector bit (bit 15) clear because CMP <= 6.
"""

from harness import Simulator, Reg, Csr, Cmp, CauseReason, CauseTypeInstruction


def _jmp_gp(sim: Simulator, cmp_value: int, target: int, flags: int) -> None:
    """Places `JMP <cmp> RA RB` at 0x0000 with RA:RB = target and FLAGS = flags, then steps once."""
    sim.set_reg(Reg.RA, target >> 8)
    sim.set_reg(Reg.RB, target & 0xFF)
    sim.set_csr(Csr.FLAGS, flags)
    sim.mem[0x0000] = 0xF | (int(Reg.RA) << 4)
    sim.mem[0x0001] = int(Reg.RB) | (cmp_value << 4)
    sim.pc = 0x0000
    sim.in_reset = False
    sim.step()


def test_not_equal_encoding_is_two_and_branches_when_z_clear():
    sim = Simulator()
    _jmp_gp(sim, int(Cmp.NOT_EQUAL), 0x1234, flags = 0b0000)  # Z=0
    assert int(Cmp.NOT_EQUAL) == 2
    assert sim.pc == 0x1234


def test_not_equal_does_not_branch_when_z_set():
    sim = Simulator()
    _jmp_gp(sim, int(Cmp.NOT_EQUAL), 0x1234, flags = 0b0001)  # Z=1
    assert sim.pc == 0x0002  # fell through


def test_all_flag_values_match_dense_encoding():
    assert (int(Cmp.ALWAYS), int(Cmp.EQUAL), int(Cmp.NOT_EQUAL), int(Cmp.GREATER),
            int(Cmp.LESS), int(Cmp.GREATER_EQUAL), int(Cmp.LESS_EQUAL)) == (0, 1, 2, 3, 4, 5, 6)


def test_reserved_comparison_faults_illegal():
    sim = Simulator()
    sim.mem[0xFFFC] = 0x00
    sim.mem[0xFFFD] = 0x00
    sim.mem[0x0000] = 0xF          # JMP ...
    sim.mem[0x0001] = 0x70         # CMP nibble 0b0111 (7, reserved), selector 0
    sim.pc = 0x0000
    sim.in_reset = False
    sim.step()
    cause = sim.get_csr(Csr.CAUSE)
    assert cause & 0b111 == CauseReason.INSTRUCTION
    assert (cause >> 3) & 0b111 == CauseTypeInstruction.ILLEGAL
