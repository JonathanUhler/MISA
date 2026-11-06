from amaranth import Cat, Module, Signal, Value
from amaranth.lib.wiring import Component, In, Out

from .grammar import Cmp


class ComparatorUnit(Component):

    z: In(1)
    c: In(1)
    n: In(1)
    v: In(1)
    cmp: In(Cmp, init = Cmp.ALWAYS)

    taken: Out(1)


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        with m.Switch(self.cmp):
            with m.Case(Cmp.ALWAYS):
                m.d.comb += self.taken.eq(1)
            with m.Case(Cmp.EQUAL):
                m.d.comb += self.taken.eq(self.z)
            with m.Case(Cmp.NOT_EQUAL):
                m.d.comb += self.taken.eq(~self.z)
            with m.Case(Cmp.GREATER):
                m.d.comb += self.taken.eq(~((self.n ^ self.v) | self.z))
            with m.Case(Cmp.LESS):
                m.d.comb += self.taken.eq(self.n ^ self.v)
            with m.Case(Cmp.GREATER_EQUAL):
                m.d.comb += self.taken.eq(~(self.n ^ self.v))
            with m.Case(Cmp.LESS_EQUAL):
                m.d.comb += self.taken.eq((self.n ^ self.v) | self.z)

        return m

