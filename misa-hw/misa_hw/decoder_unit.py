from amaranth import Module, Signal, Value
from amaranth.lib.wiring import Component, In, Out

from .grammar import Op, CsrReg, CmpFlag, in_enum


class DecoderUnit(Component):

    inst: In(16)

    op: Out(Op)
    n1: Out(4)
    n2: Out(4)
    n3: Out(4)
    imm4: Out(4)
    imm8: Out(8)
    cmp: Out(3)
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
            self.is_jmp_csr.eq(self.inst[15]),
            self.is_syscall.eq(self.inst[15])
        ]

        is_illegal_csr: Signal = ((op == Op.RSR) | (op == Op.WSR)) & ~in_enum(self.n3, CsrReg)
        is_illegal_jmp: Signal = (op == Op.JMP) & self.is_jmp_csr & ~in_enum(self.n1, CsrReg)
        is_illegal_cmp: Signal = ((op == Op.JAL) | (op == Op.JMP)) & ~in_enum(self.cmp, CmpFlag)
        m.d.comb += self.is_illegal(is_illegal_csr | is_illegal_jmp | is_illegal_cmp)

        return m
    

    
