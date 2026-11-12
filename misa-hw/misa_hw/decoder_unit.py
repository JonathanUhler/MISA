from amaranth import Module, Mux, Signal, Value
from amaranth.lib.wiring import Component, In, Out

from .grammar import CmpFlag, CsrReg, Extn, Op, in_enum


class DecoderUnit(Component):

    inst: In(16)
    extns: In(4)

    op: Out(Op, init = Op.HALT)
    n1: Out(4)
    n2: Out(4)
    n3: Out(4)
    imm4: Out(4)
    imm8: Out(8)
    cmp: Out(3)
    csr: Out(CsrReg, init = CsrReg.SADDR)
    csr_user_readable: Out(1)
    csr_user_writable: Out(1)
    is_jmp_csr: Out(1)
    is_syscall: Out(1)
    is_illegal: Out(1)


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        m.d.comb += [
            self.op.eq(self.inst[0:4]),
            self.n1.eq(self.inst[4:8]),
            self.n2.eq(self.inst[8:12]),
            self.n3.eq(self.inst[12:16]),
            self.imm4.eq(self.inst[12:16]),
            self.imm8.eq(self.inst[8:16]),
            self.cmp.eq(self.inst[12:15]),
            self.is_jmp_csr.eq((self.op == Op.JMP) & self.inst[15]),
            self.is_syscall.eq((self.op == Op.HALT) & self.inst[15])
        ]

        m.d.comb += self.csr.eq(Mux(self.is_jmp_csr, self.n1, self.n3))

        csr_available: Signal = Signal(1)
        with m.Switch(self.csr):
            with m.Case(
                CsrReg.SADDR, CsrReg.RADDR, CsrReg.FLAGS, CsrReg.CAUSE, CsrReg.EXTNS, CsrReg.FAULT
            ):
                m.d.comb += csr_available.eq(1)
            with m.Case(CsrReg.RETSC):
                m.d.comb += csr_available.eq((self.extns & Extn.SYSCALL).any())
            with m.Case(CsrReg.RETIR):
                m.d.comb += csr_available.eq((self.extns & Extn.INTERRUPT).any())
            with m.Case(CsrReg.PRIVS):
                m.d.comb += csr_available.eq((self.extns & Extn.PRIVILEGE).any())
            with m.Default():
                m.d.comb += csr_available.eq(0)

        with m.Switch(self.csr):
            with m.Case(CsrReg.SADDR, CsrReg.RADDR, CsrReg.FLAGS):
                m.d.comb += [
                    self.csr_user_readable.eq(1),
                    self.csr_user_writable.eq(1)
                ]
            with m.Case(CsrReg.PRIVS):
                m.d.comb += self.csr_user_readable.eq(1)

        is_illegal_halt_resv: Value = (self.op == Op.HALT) & (self.inst[8:15] != 0)
        is_illegal_rrc_resv: Value  = (self.op == Op.RRC) & (self.n3 != 0)
        is_illegal_jal_resv: Value  = (self.op == Op.JAL) & self.inst[15]
        is_illegal_jmp_resv: Value  = self.is_jmp_csr & (self.n2 != 0)

        is_illegal_csr: Value = \
            ((self.op == Op.RSR) | (self.op == Op.WSR) | self.is_jmp_csr) & ~csr_available
        is_illegal_cmp: Value = \
            ((self.op == Op.JAL) | (self.op == Op.JMP)) & ~in_enum(self.cmp, CmpFlag)
        is_illegal_syscall: Value = \
            self.is_syscall & ~(self.extns & Extn.SYSCALL).any()

        m.d.comb += self.is_illegal.eq(
            is_illegal_halt_resv | is_illegal_rrc_resv | is_illegal_jal_resv | is_illegal_jmp_resv |
            is_illegal_csr | is_illegal_cmp | is_illegal_syscall
        )

        return m
