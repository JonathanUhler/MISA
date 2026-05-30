from enum import IntEnum


INST_SIZE: int = 16
CSR_SIZE: int  = 16
WORD_SIZE: int =  8
NIB_SIZE: int  =  4

CSR_MASK: int  = 2 ** CSR_SIZE - 1
WORD_MASK: int = 2 ** WORD_SIZE - 1
SIGN_MASK: int = 2 ** (WORD_SIZE - 1)


class Op(IntEnum):
    SYSCALL = 0x0
    ADD     = 0x1
    ADC     = 0x2
    SUB     = 0x3
    SBB     = 0x4
    AND     = 0x5
    OR      = 0x6
    XOR     = 0x7
    RRC     = 0x8
    SET     = 0x9
    LD      = 0xA
    ST      = 0xB
    RSR     = 0xC
    WSR     = 0xD
    JAL     = 0xE
    JMP     = 0xF


class Reg(IntEnum):
    R0        = 0x0
    RA        = 0x1
    RB        = 0x2
    RC        = 0x3
    RD        = 0x4
    RE        = 0x5
    RF        = 0x6
    RU        = 0x7
    RV        = 0x8
    RW        = 0x9
    RX        = 0xA
    RY        = 0xB
    RZ        = 0xC
    RT        = 0xD
    RSCRATCH0 = 0xE
    RSCRATCH1 = 0xF


class Csr(IntEnum):
    SADDR = 0x1
    RADDR = 0x2
    FLAGS = 0x3
    CAUSE = 0x4
    EXTNS = 0x5
    RETSC = 0x8
    PRIVS = 0xA


class Cmp(IntEnum):
    ALWAYS        = 0x0
    EQUAL         = 0x1
    NOT_EQUAL     = 0x8
    GREATER       = 0x2
    LESS          = 0x4
    GREATER_EQUAL = 0x3
    LESS_EQUAL    = 0x5


class Vector(IntEnum):
    SYSCALL = 0xFFFA
    FAULT   = 0xFFFC
    RESET   = 0xFFFE


class CauseReason(IntEnum):
    NONE        = 0x0
    INSTRUCTION = 0x1
    MEMORY      = 0x2


class CauseTypeInstruction(IntEnum):
    HALT       = 0x0
    ILLEGAL    = 0x1
    PRIVILEGED = 0x2


class CauseTypeMemory(IntEnum):
    ADDRESS    = 0x0
    PC         = 0x1
    PRIVILEGED = 0x2


class Simulator:

    def _FaultSignal(Exception):
        pass


    def __init__(self) -> None:
        self.in_reset = True
        self.in_syscall = False
        self._saved_privs = 0x0000
        self.pc = 0x0000
        self.reg = [0] * 16
        self.csr = [0] * 16
        self.csr[Csr.EXTNS] = 0b110
        self.mem = [0] * 0x10000
        self.reset()


    def _fault(self, extended_status: int,
               cause_type: CauseTypeInstruction | CauseTypeMemory,
               cause_reason: CauseReason) -> None:
        self._set_cause(extended_status, cause_type, cause_reason)
        self._set_csr(Csr.PRIVS, 0x0000)
        self.in_syscall = False
        self.pc = (self._read_mem(Vector.FAULT + 1) << WORD_SIZE) | self._read_mem(Vector.FAULT)
        raise _FaultSignal()


    def _compare(self, cmp: Cmp | int) -> bool:
        if (not isinstance(cmp, Cmp)):
            try:
                cmp = Cmp(cmp)
            except ValueError:
                self._fault(0x00, CauseTypeInstruction.ILLEGAL, CauseReason.INSTRUCTION)

        flags: int = self._get_csr(Csr.FLAGS)
        z: bool = bool(flags & 0b0001)
        n: bool = bool(flags & 0b0100)
        v: bool = bool(flags & 0b1000)

        match (cmp):
            case Cmp.ALWAYS:
                return True
            case Cmp.EQUAL:
                return z
            case Cmp.NOT_EQUAL:
                return not z
            case Cmp.GREATER:
                return not ((n ^ v) or z)
            case Cmp.LESS:
                return n ^ v
            case Cmp.GREATER_EQUAL:
                return not (n ^ v)
            case Cmp.LESS_EQUAL:
                return (n ^ v) or z


    def _set_cause(self, extended_status: int,
                   cause_type: CauseTypeInstruction | CauseTypeMemory,
                   cause_reason: CauseReason) -> None:
        cause: int = (extended_status << WORD_SIZE) | (cause_type << 3) | (cause_reason)
        self._set_csr(Csr.CAUSE, cause)


    def _set_flags(self, a: int, b: int, y: int) -> None:
        a_sign: bool = bool(a & SIGN_MASK)
        b_sign: bool = bool(b & SIGN_MASK)
        y_sign: bool = bool(y & SIGN_MASK)

        z: bool = y == 0
        c: bool = bool(y & (WORD_MASK + 1))
        n: bool = y_sign
        v: bool = (not (a_sign ^ b_sign)) and (b_sign ^ y_sign)
        self._set_csr(Csr.FLAGS, (v << 3) | (n << 2) | (c << 1) | (z << 0))


    def _is_privileged(self) -> bool:
        return bool(self._get_csr(Csr.PRIVS) & 0b0001 == 0)


    def _get_csr(self, csr: Csr | int) -> int:
        if (not isinstance(csr, Csr)):
            try:
                csr = Csr(csr)
            except ValueError:
                self._fault(0x00, CauseTypeInstruction.ILLEGAL, CauseReason.INSTRUCTION)
        return self.csr[csr]


    def _set_csr(self, csr: Csr | int, value: int) -> None:
        if (not isinstance(csr, Csr)):
            try:
                csr = Csr(csr)
            except ValueError:
                self._fault(0x00, CauseTypeInstruction.ILLEGAL, CauseReason.INSTRUCTION)
        if (csr == Csr.EXTNS):
            return
        self.csr[csr] = value % (CSR_MASK + 1)


    def _get_reg(self, reg: Reg | int) -> int:
        if (not isinstance(reg, Reg)):
            try:
                reg = Reg(reg)
            except ValueError:
                self._fault(0x00, CauseTypeInstruction.ILLEGAL, CauseReason.INSTRUCTION)
        return self.reg[reg]


    def _set_reg(self, reg: Reg | int, value: int) -> None:
        if (not isinstance(reg, Reg)):
            try:
                reg = Reg(reg)
            except ValueError:
                self._fault(0x00, CauseTypeInstruction.ILLEGAL, CauseReason.INSTRUCTION)
        if (reg == Reg.R0):
            return
        self.reg[reg] = value % (WORD_MASK + 1)


    def _read_mem(self, address: int) -> int:
        return self.mem[address]


    def _write_mem(self, address: int, value: int) -> None:
        self.mem[address] = value % (WORD_MASK + 1)


    def reset(self) -> None:
        self.in_reset = False
        self.in_syscall = False
        self._saved_privs = 0x0000
        self.pc = (self._read_mem(Vector.RESET + 1) << WORD_SIZE) | self._read_mem(Vector.RESET)


    def step(self):
        try:
            inst: int = (self._read_mem(self.pc + 1) << WORD_SIZE) | self._read_mem(self.pc)
            nib0: int = (inst & 0x000F) >> (0 * NIB_SIZE)
            nib1: int = (inst & 0x00F0) >> (1 * NIB_SIZE)
            nib2: int = (inst & 0x0F00) >> (2 * NIB_SIZE)
            nib3: int = (inst & 0xF000) >> (3 * NIB_SIZE)

            self.pc += INST_SIZE // WORD_SIZE

            match (Op(nib0)):
                case Op.SYSCALL: self._syscall(nib1, bool(nib3 & 0b1000))
                case Op.ADD:     self._add(nib1, nib2, nib3)
                case Op.ADC:     self._adc(nib1, nib2, nib3)
                case Op.SUB:     self._sub(nib1, nib2, nib3)
                case Op.SBB:     self._sbb(nib1, nib2, nib3)
                case Op.AND:     self._and(nib1, nib2, nib3)
                case Op.OR:      self._or(nib1, nib2, nib3)
                case Op.XOR:     self._xor(nib1, nib2, nib3)
                case Op.RRC:     self._rrc(nib1, nib2)
                case Op.SET:     self._set(nib1, (nib3 << NIB_SIZE) | nib2)
                case Op.LD:      self._ld(nib1, nib2, nib3)
                case Op.ST:      self._st(nib1, nib2, nib3)
                case Op.RSR:     self._rsr(nib1, nib2, nib3)
                case Op.WSR:     self._wsr(nib1, nib2, nib3)
                case Op.JAL:     self._jal(nib1, nib2, nib3)
                case Op.JMP:     self._jmp(nib1, nib2, nib3)

            if (self.in_syscall and self.pc == self._get_csr(Csr.RETSC)):
                self.in_syscall = False
                self._set_csr(Csr.PRIVS, self._saved_privs)
        except _FaultSignal:
            pass


    def _syscall(self, rs: int, e: bool) -> None:
        if (self.in_syscall):
            self._fault(0x00, CauseTypeInstruction.ILLEGAL, CauseReason.INSTRUCTION)

        if (not e and not self._is_privileged()):
            self._fault(0x00, CauseTypeInstruction.PRIVILEGED, CauseReason.INSTRUCTION)

        self._set_cause(self._get_reg(rs), CauseTypeInstruction.HALT, CauseReason.INSTRUCTION)
        if (not e):
            self.in_reset = True
        else:
            self.in_syscall = True
            self._saved_privs = self._get_csr(Csr.PRIVS)
            self._set_csr(Csr.PRIVS, 0x0000)
            self._set_csr(Csr.RETSC, self.pc)
            self.pc = \
                (self._read_mem(Vector.SYSCALL + 1) << WORD_SIZE) | self._read_mem(Vector.SYSCALL)


    def _add(self, rd: int, rs1: int, rs2: int) -> None:
        a: int = self._get_reg(rs1)
        b: int = self._get_reg(rs2)
        y: int = a + b
        self._set_flags(a, b, y)
        self._set_reg(rd, y)


    def _adc(self, rd: int, rs1: int, rs2: int) -> None:
        a: int = self._get_reg(rs1)
        b: int = self._get_reg(rs2)
        c: bool = bool(self._get_csr(Csr.FLAGS) & 0b0010)
        y: int = a + b + c
        self._set_flags(a, b, y)
        self._set_reg(rd, y)


    def _sub(self, rd: int, rs1: int, rs2: int) -> None:
        a: int = self._get_reg(rs1)
        b: int = self._get_reg(rs2)
        y: int = a - b
        self._set_flags(a, ~b & WORD_MASK, y)
        self._set_reg(rd, y)


    def _sbb(self, rd: int, rs1: int, rs2: int) -> None:
        a: int = self._get_reg(rs1)
        b: int = self._get_reg(rs2)
        c: bool = bool(self._get_csr(Csr.FLAGS) & 0b0010)
        y: int = a - (b + c)
        self._set_flags(a, ~b & WORD_MASK, y)
        self._set_reg(rd, y)


    def _and(self, rd: int, rs1: int, rs2: int) -> None:
        a: int = self._get_reg(rs1)
        b: int = self._get_reg(rs2)
        y: int = a & b
        self._set_reg(rd, y)


    def _or(self, rd: int, rs1: int, rs2: int) -> None:
        a: int = self._get_reg(rs1)
        b: int = self._get_reg(rs2)
        y: int = a | b
        self._set_reg(rd, y)


    def _xor(self, rd: int, rs1: int, rs2: int) -> None:
        a: int = self._get_reg(rs1)
        b: int = self._get_reg(rs2)
        y: int = a ^ b
        self._set_reg(rd, y)


    def _rrc(self, rd: int, rs: int) -> None:
        a: int = self._get_reg(rs)
        c: bool = bool(self._get_csr(Csr.FLAGS) & 0b0010)
        y: int = (c << (WORD_SIZE - 1)) | (a >> 1)
        self._set_flags(0, 0, a << WORD_SIZE)
        self._set_reg(rd, y)


    def _set(self, rd: int, imm: int) -> None:
        self._set_reg(rd, imm)


    def _ld(self, rd: int, rs1: int, rs2: int) -> None:
        address: int = (self._get_reg(rs1) << WORD_SIZE) | self._get_reg(rs2)
        self._set_reg(rd, self._read_mem(address))


    def _st(self, rd: int, rs1: int, rs2: int) -> None:
        address: int = (self._get_reg(rs1) << WORD_SIZE) | self._get_reg(rs2)
        self._write_mem(address, self._get_reg(rd))


    def _rsr(self, rs1: int, rs2: int, csr: int) -> None:
        if (not self._is_privileged() and csr not in {Csr.SADDR, Csr.RADDR, Csr.FLAGS}):
            self._fault(0x00, CauseTypeInstruction.PRIVILEGED, CauseReason.INSTRUCTION)
        value: int = self._get_csr(csr)
        self._set_reg(rs1, value >> WORD_SIZE)
        self._set_reg(rs2, value & WORD_MASK)


    def _wsr(self, rs1: int, rs2: int, csr: int) -> None:
        if (not self._is_privileged() and csr not in {Csr.SADDR, Csr.RADDR, Csr.FLAGS}):
            self._fault(0x00, CauseTypeInstruction.PRIVILEGED, CauseReason.INSTRUCTION)
        value: int = (self._get_reg(rs1) << WORD_SIZE) | self._get_reg(rs2)
        self._set_csr(csr, value)


    def _jal(self, rs1: int, rs2: int, cmp: int) -> None:
        if (self._compare(cmp)):
            self._set_csr(Csr.RADDR, self.pc)
            self.pc = (self._get_reg(rs1) << WORD_SIZE) | self._get_reg(rs2)


    def _jmp(self, rs1: int, rs2: int, cmp: int) -> None:
        if (self._compare(cmp)):
            self.pc = (self._get_reg(rs1) << WORD_SIZE) | self._get_reg(rs2)
