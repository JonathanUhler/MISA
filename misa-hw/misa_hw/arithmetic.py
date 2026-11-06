from amaranth import Cat, Module, Signal, Value
from amaranth.lib.wiring import Component, In, Out

from .grammar import ArithmeticOp


class ArithmeticUnit(Component):

    op: In(ArithmeticOp, init = ArithmeticOp.ADD)
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

        effective_b: Signal = Signal(8)
        effective_cin: Signal = Signal(1)
        is_sub: Value = (self.op == ArithmeticOp.SUB) | (self.op == ArithmeticOp.SBB)
        m.d.comb += effective_b.eq(self.b ^ is_sub.replicate(8))
        with m.Switch(self.op):
            with m.Case(ArithmeticOp.SUB):
                m.d.comb += effective_cin.eq(1)
            with m.Case(ArithmeticOp.ADC, ArithmeticOp.SBB):
                m.d.comb += effective_cin.eq(self.cin)
            with m.Default():
                m.d.comb += effective_cin.eq(0)

        wide: Signal = Signal(9)
        m.d.comb += wide.eq(self.a + effective_b + effective_cin)
        add_result: Value = wide[0 : 8]
        add_c: Value = wide[8]
        add_v: Value = (self.a[7] == effective_b[7]) & (effective_b[7] != add_result[7])
        rrc_result: Value = Cat(self.a[1 : 8], self.cin)

        with m.Switch(self.op):
            with m.Case(ArithmeticOp.ADD, ArithmeticOp.ADC, ArithmeticOp.SUB, ArithmeticOp.SBB):
                m.d.comb += [self.y.eq(add_result), self.c.eq(add_c), self.v.eq(add_v)]
            with m.Case(ArithmeticOp.AND):
                m.d.comb += self.y.eq(self.a & self.b)
            with m.Case(ArithmeticOp.OR):
                m.d.comb += self.y.eq(self.a | self.b)
            with m.Case(ArithmeticOp.XOR):
                m.d.comb += self.y.eq(self.a ^ self.b)
            with m.Case(ArithmeticOp.RRC):
                m.d.comb += [self.y.eq(rrc_result), self.c.eq(self.a[0])]

        m.d.comb += [self.z.eq(self.y == 0), self.n.eq(self.y[7])]
        return m

