from amaranth import Cat, Module, Signal, Value
from amaranth.lib.data import StructLayout
from amaranth.lib.wiring import Component, In, Out

from .grammar import Csr, Op


DecodedLayout: StructLayout = StructLayout({
    "op": Op,
    "n1": 4,
    "n2": 4,
    "n3": 4,
    "imm8": 8,
    "imm4": 4,
    "cmp": 3,
    "csr_target": 1,
    "is_syscall": 1,
    "illegal": 1
})


class Decoder(Component):

    inst: In(16)

    out: Out(DecodedLayout)


    def _valid_csr(self, x: Value) -> Value:
        return ((x == Csr.SADDR) | (x == Csr.RADDR) | (x == Csr.FLAGS) | (x == Csr.CAUSE) |
                (x == Csr.EXTNS) | (x == Csr.RETSC) | (x == Csr.RETIR) | (x == Csr.PRIVS))


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        inst: Value = self.inst
        op: Value = inst[0 : 4]

        m.d.comb += [
            self.out.op.eq(op),
            self.out.n1.eq(inst[4 : 8]),
            self.out.n2.eq(inst[8 : 12]),
            self.out.n3.eq(inst[12 : 16]),
            self.out.imm8.eq(inst[8 : 16]),
            self.out.imm4.eq(inst[12 : 16]),
            self.out.cmp.eq(inst[12 : 15]),
            self.out.csr_target.eq(inst[15]),
            self.out.is_syscall.eq(inst[15])
        ]

        illegal_csr: Value = ((op == Op.RSR) | (op == Op.WSR)) & ~self._valid_csr(self.out.n3)
        illegal_jmp: Value = (op == Op.JMP) & self.out.csr_target & ~self._valid_csr(self.out.n1)
        illegal_cmp: Value = ((op == Op.JAL) | (op == Op.JMP)) & (self.out.cmp == 0b111)
        m.d.comb += self.out.illegal.eq(illegal_csr | illegal_jmp | illegal_cmp)

        return m
