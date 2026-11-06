from dataclasses import astuple

from amaranth import Array, Cat, Module, Signal, Value
from amaranth.lib.wiring import Component, In, Out

from .grammar import Reg
from .init import RegInit


class RegFile(Component):

    read_reg: In(Reg, init = Reg.R0)
    write_reg: In(Reg, init = Reg.R0)
    write_data: In(8)
    write_enable: In(1)

    read_data: Out(8)


    def __init__(self, init_data: RegInit = None) -> None:
        if (init_data is None):
            init_data = RegInit()
        self.regs: Array = Array([Signal(8, init = data) for data in [0, *astuple(init_data)]])
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        with m.If(self.read_reg == Reg.R0):
            m.d.comb += self.read_data.eq(0)
        with m.Else():
            m.d.comb += self.read_data.eq(self.regs[self.read_reg])

        with m.If(self.write_enable & (self.write_reg != Reg.R0)):
            m.d.sync += self.regs[self.write_reg].eq(self.write_data)

        return m
