from amaranth import Cat, Module, Signal
from amaranth.lib.wiring import Component, In, Out

from .grammar import AluOp


class AluUnit(Component):

    alu_op: In(AluOp, init = AluOp.ADD)
    a: In(8)
    b: In(8)
    cin: In(1)

    y: Out(8)
    z: Out(1)
    c: Out(1)
    n: Out(1)
    v: Out(1)


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        y_full: Signal = Signal(9)
        eff_b: Signal = Signal(8)
        with m.Switch(self.alu_op):
            with m.Case(AluOp.SUB, AluOp.SBB):
                m.d.comb += eff_b.eq(~self.b)
            with m.Default():
                m.d.comb += eff_b.eq(self.b)

        with m.Switch(self.alu_op):
            with m.Case(AluOp.ADD):
                m.d.comb += y_full.eq(self.a + eff_b)
            with m.Case(AluOp.ADC):
                m.d.comb += y_full.eq(self.a + eff_b + self.cin)
            with m.Case(AluOp.SUB):
                m.d.comb += y_full.eq(self.a + eff_b + 1)
            with m.Case(AluOp.SBB):
                m.d.comb += y_full.eq(self.a + eff_b + self.cin)
            with m.Case(AluOp.AND):
                m.d.comb += y_full.eq(self.a & eff_b)
            with m.Case(AluOp.OR):
                m.d.comb += y_full.eq(self.a | eff_b)
            with m.Case(AluOp.XOR):
                m.d.comb += y_full.eq(self.a ^ eff_b)
            with m.Case(AluOp.RRC):
                m.d.comb += y_full.eq(Cat(self.a[1:8], self.cin))

        m.d.comb += [
            self.y.eq(y_full[0:8]),
            self.z.eq(self.y == 0x00),
            self.n.eq(y_full[7])
        ]
        with m.If(self.alu_op == AluOp.RRC):
            m.d.comb += self.c.eq(self.a[0])
        with m.Else():
            m.d.comb += self.c.eq(y_full[8])
        with m.Switch(self.alu_op):
            with m.Case(AluOp.AND, AluOp.OR, AluOp.XOR, AluOp.RRC):
                m.d.comb += self.v.eq(0)
            with m.Default():
                m.d.comb += self.v.eq((self.a[7] == eff_b[7]) & (self.a[7] != y_full[7]))

        return m
