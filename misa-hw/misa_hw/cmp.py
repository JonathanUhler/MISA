"""The branch comparison unit, evaluating the seven conditions from misa-spec's `CsrFlags.eval`."""

from amaranth import Module, Value
from amaranth.lib import wiring
from amaranth.lib.wiring import In, Out

from .types import Cmp, FLAG_Z, FLAG_C, FLAG_N, FLAG_V


class CmpUnit(wiring.Component):
    flags: In(4)
    sel: In(Cmp)
    taken: Out(1)

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()
        z: Value = self.flags[FLAG_Z]
        n: Value = self.flags[FLAG_N]
        v: Value = self.flags[FLAG_V]
        nv: Value = n ^ v
        with m.Switch(self.sel):
            with m.Case(Cmp.ALWAYS):
                m.d.comb += self.taken.eq(1)
            with m.Case(Cmp.EQUAL):
                m.d.comb += self.taken.eq(z)
            with m.Case(Cmp.NOT_EQUAL):
                m.d.comb += self.taken.eq(~z)
            with m.Case(Cmp.GREATER):
                m.d.comb += self.taken.eq(~(nv | z))
            with m.Case(Cmp.LESS):
                m.d.comb += self.taken.eq(nv)
            with m.Case(Cmp.GREATER_EQUAL):
                m.d.comb += self.taken.eq(~nv)
            with m.Case(Cmp.LESS_EQUAL):
                m.d.comb += self.taken.eq(nv | z)
        return m
