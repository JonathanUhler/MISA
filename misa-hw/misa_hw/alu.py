"""The 8-bit ALU, built around a single 9-bit adder shared by the arithmetic operations.

Mirrors misa-spec's `aluAdd` and the logic/shift flag rules in `Execute.lean`.
"""

from amaranth import Module, Signal, Cat, Value
from amaranth.lib import wiring
from amaranth.lib.wiring import In, Out

from .types import AluOp


class Alu(wiring.Component):
    op: In(AluOp, init=AluOp.ADD)
    a: In(8)
    b: In(8)
    carry_in: In(1)  # the current FLAGS.C
    result: Out(8)
    z: Out(1)
    c: Out(1)
    n: Out(1)
    v: Out(1)

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        # Adder path: SUB/SBB invert b; carry-in is 0 (ADD), 1 (SUB), or FLAGS.C (ADC/SBB).
        b_eff: Signal = Signal(8)
        cin: Signal = Signal()
        is_sub: Value = (self.op == AluOp.SUB) | (self.op == AluOp.SBB)
        m.d.comb += b_eff.eq(self.b ^ is_sub.replicate(8))
        with m.Switch(self.op):
            with m.Case(AluOp.SUB):
                m.d.comb += cin.eq(1)
            with m.Case(AluOp.ADC, AluOp.SBB):
                m.d.comb += cin.eq(self.carry_in)
            with m.Default():
                m.d.comb += cin.eq(0)

        wide: Signal = Signal(9)
        m.d.comb += wide.eq(self.a + b_eff + cin)
        add_res: Value = wide[0:8]
        add_c: Value = wide[8]
        add_v: Value = (self.a[7] == b_eff[7]) & (b_eff[7] != add_res[7])

        # RRC: result = {carry_in, a[7:1]}, carry out = a[0].
        rrc_res: Value = Cat(self.a[1:8], self.carry_in)

        with m.Switch(self.op):
            with m.Case(AluOp.ADD, AluOp.ADC, AluOp.SUB, AluOp.SBB):
                m.d.comb += [self.result.eq(add_res), self.c.eq(add_c), self.v.eq(add_v)]
            with m.Case(AluOp.AND):
                m.d.comb += self.result.eq(self.a & self.b)
            with m.Case(AluOp.OR):
                m.d.comb += self.result.eq(self.a | self.b)
            with m.Case(AluOp.XOR):
                m.d.comb += self.result.eq(self.a ^ self.b)
            with m.Case(AluOp.RRC):
                m.d.comb += [self.result.eq(rrc_res), self.c.eq(self.a[0])]

        m.d.comb += [self.z.eq(self.result == 0), self.n.eq(self.result[7])]
        return m
