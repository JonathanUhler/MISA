"""Independent Python golden models mirroring the ISA chapter (docs/arch/isa.qmd), not the RTL.

Only the instruction/CSR *encodings* are shared with the implementation (imported from grammar, the
single source of truth). The decode slicing, ALU math, comparison logic, and CSR write rules are
re-derived here from the ISA chapter, so the models stay independent of the RTL under test.
"""

from misa_hw.grammar import ArithmeticOp, Cmp, Csr, Extn, Op


# Valid special-register (CSR) operand encodings.
_VALID_CSR: set = {int(csr) for csr in Csr}


def decode(word: int) -> dict:
    """Slice a 16-bit instruction word into its fields and flag structurally illegal encodings."""
    op: int = word & 0xF
    n1: int = (word >> 4) & 0xF
    n2: int = (word >> 8) & 0xF
    n3: int = (word >> 12) & 0xF
    imm8: int = (word >> 8) & 0xFF
    imm4: int = (word >> 12) & 0xF
    cmp: int = (word >> 12) & 0x7
    csr_target: int = (word >> 15) & 0x1
    is_syscall: int = (word >> 15) & 0x1

    illegal_csr: bool = op in (Op.RSR, Op.WSR) and n3 not in _VALID_CSR
    illegal_jmp: bool = op == Op.JMP and csr_target == 1 and n1 not in _VALID_CSR
    illegal_cmp: bool = op in (Op.JAL, Op.JMP) and cmp == 0b111
    illegal: int = int(illegal_csr or illegal_jmp or illegal_cmp)

    return {"op": op, "n1": n1, "n2": n2, "n3": n3, "imm8": imm8, "imm4": imm4,
            "cmp": cmp, "csr_target": csr_target, "is_syscall": is_syscall, "illegal": illegal}


def comparator(cmp: int, z: int, c: int, n: int, v: int) -> int:
    """The branch-taken predicate for each comparison (isa.qmd FLAGS table). Unused code -> 0."""
    z &= 1
    nv: int = (n ^ v) & 1
    if (cmp == Cmp.ALWAYS):
        return 1
    if (cmp == Cmp.EQUAL):            # Z
        return z
    if (cmp == Cmp.NOT_EQUAL):        # ~Z
        return z ^ 1
    if (cmp == Cmp.GREATER):          # ~((N ^ V) | Z)
        return (nv | z) ^ 1
    if (cmp == Cmp.LESS):             # N ^ V
        return nv
    if (cmp == Cmp.GREATER_EQUAL):    # ~(N ^ V)
        return nv ^ 1
    if (cmp == Cmp.LESS_EQUAL):       # (N ^ V) | Z
        return nv | z
    return 0                          # 0b111 is not a defined comparison


def alu(op: int, a: int, b: int, cin: int) -> tuple:
    """8-bit ALU result and Z,C,N,V flags per isa.qmd. Returns (y, z, c, n, v)."""
    a &= 0xFF
    b &= 0xFF
    cin &= 1
    c: int = 0
    v: int = 0
    if op in (ArithmeticOp.ADD, ArithmeticOp.ADC, ArithmeticOp.SUB, ArithmeticOp.SBB):
        is_sub: bool = op in (ArithmeticOp.SUB, ArithmeticOp.SBB)
        eff_b: int = b ^ (0xFF if is_sub else 0)
        if (op == ArithmeticOp.SUB):
            eff_cin: int = 1
        elif op in (ArithmeticOp.ADC, ArithmeticOp.SBB):
            eff_cin = cin
        else:                                      # ADD
            eff_cin = 0
        wide: int = a + eff_b + eff_cin
        y: int = wide & 0xFF
        c = (wide >> 8) & 1
        # Signed overflow: operands share a sign that differs from the result's sign.
        v = int((a >> 7) == (eff_b >> 7) and (eff_b >> 7) != (y >> 7))
    elif op == ArithmeticOp.AND:
        y = a & b
    elif op == ArithmeticOp.OR:
        y = a | b
    elif op == ArithmeticOp.XOR:
        y = a ^ b
    elif op == ArithmeticOp.RRC:
        y = ((cin << 7) | (a >> 1)) & 0xFF        # shift {C, a} right by one
        c = a & 1
    else:
        y = 0
    z: int = int(y == 0)
    n: int = (y >> 7) & 1
    return (y, z, c, n, v)


class CsrModel:
    """Model of CsrFile write/read semantics (isa.qmd 'Control and Status Registers')."""

    def __init__(self, extns: int = 0x0F) -> None:
        self.saddr: int = 0
        self.raddr: int = 0
        self.flags: int = 0        # 4-bit
        self.cause: int = 0
        self.extns: int = extns & 0xF
        self.retsc: int = 0
        self.retir: int = 0
        self.privs: int = 0        # 1-bit

    def read(self, reg: int) -> int:
        return {
            Csr.SADDR: self.saddr, Csr.RADDR: self.raddr, Csr.FLAGS: self.flags,
            Csr.CAUSE: self.cause, Csr.EXTNS: self.extns, Csr.RETSC: self.retsc,
            Csr.RETIR: self.retir, Csr.PRIVS: self.privs,
        }.get(reg, 0)

    def step(self, write_reg: int, write_enable: int, write_data: int,
             flags_write_enable: int, flags_write_data: int) -> None:
        """One clock edge: a general CSR write, then the dedicated FLAGS write (which wins)."""
        if write_enable:
            if write_reg == Csr.SADDR:
                self.saddr = write_data & 0xFFFF
            elif write_reg == Csr.RADDR:
                self.raddr = write_data & 0xFFFF
            elif write_reg == Csr.FLAGS:
                self.flags = write_data & 0xF
            elif write_reg == Csr.CAUSE:
                self.cause = write_data & 0xFFFF
            elif write_reg == Csr.EXTNS:
                if self.extns & Extn.DYNAMICHW:                        # writable only when DHW is live
                    self.extns = ((write_data & 0b1110) | Extn.DYNAMICHW) & 0xF   # bit 0 pinned
            elif write_reg == Csr.RETSC:
                self.retsc = write_data & 0xFFFF
            elif write_reg == Csr.RETIR:
                self.retir = write_data & 0xFFFF
            elif write_reg == Csr.PRIVS:
                self.privs = write_data & 0x1
        if flags_write_enable:
            self.flags = flags_write_data & 0xF
