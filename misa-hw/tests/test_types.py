from misa_hw.types import Opcode, Csr, Cmp, FaultKind


def test_opcode_encodings() -> None:
    assert (Opcode.HALT, Opcode.ADD, Opcode.ADC, Opcode.SUB, Opcode.SBB) == (0, 1, 2, 3, 4)
    assert (Opcode.AND, Opcode.OR, Opcode.XOR, Opcode.RRC, Opcode.SET) == (5, 6, 7, 8, 9)
    assert (Opcode.LD, Opcode.ST, Opcode.RSR, Opcode.WSR, Opcode.JAL, Opcode.JMP) == \
        (0xA, 0xB, 0xC, 0xD, 0xE, 0xF)


def test_csr_encodings() -> None:
    assert (Csr.SADDR, Csr.RADDR, Csr.FLAGS, Csr.CAUSE, Csr.EXTNS) == (1, 2, 3, 4, 5)
    assert (Csr.RETSC, Csr.RETIR, Csr.PRIVS) == (8, 9, 10)


def test_cmp_encodings() -> None:
    assert (Cmp.ALWAYS, Cmp.EQUAL, Cmp.NOT_EQUAL, Cmp.GREATER) == (0, 1, 2, 3)
    assert (Cmp.LESS, Cmp.GREATER_EQUAL, Cmp.LESS_EQUAL) == (4, 5, 6)


def test_fault_kind_encodings() -> None:  # 6-bit values from misa-spec CsrReg.lean
    assert FaultKind.HALT == 0b000001 and FaultKind.ILLEGAL == 0b001001
    assert FaultKind.PRIV_INSTRUCTION == 0b010001 and FaultKind.ADDR_OVERFLOW == 0b000010
    assert FaultKind.PC_OVERFLOW == 0b001010 and FaultKind.PRIV_ACCESS == 0b010010
    assert FaultKind.IMPROPER_RETURN == 0b000011 and FaultKind.COND_RETURN == 0b001011
    assert FaultKind.IMPROPER_SYSCALL == 0b010011
