from amaranth import Module
from amaranth.lib.wiring import Component, In, Out

from .grammar import CmpFlag


class CmpUnit(Component):

    cmp_flag: In(CmpFlag, init = CmpFlag.ALWAYS)
    z: In(1)
    c: In(1)
    n: In(1)
    v: In(1)

    true: Out(1)


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        with m.Switch(self.cmp_flag):
            with m.Case(CmpFlag.ALWAYS):
                m.d.comb += self.true.eq(1)
            with m.Case(CmpFlag.EQUAL):
                m.d.comb += self.true.eq(self.z)
            with m.Case(CmpFlag.NOT_EQUAL):
                m.d.comb += self.true.eq(~self.z)
            with m.Case(CmpFlag.BELOW):
                m.d.comb += self.true.eq(~self.c)
            with m.Case(CmpFlag.LESS):
                m.d.comb += self.true.eq(self.n ^ self.v)
            with m.Case(CmpFlag.GREATER_EQUAL):
                m.d.comb += self.true.eq(~(self.n ^ self.v))
            with m.Case(CmpFlag.ABOVE_EQUAL):
                m.d.comb += self.true.eq(self.c)
            with m.Default():
                m.d.comb += self.true.eq(0)

        return m
