from dataclasses import astuple

from amaranth import Array, Module, Signal
from amaranth.lib.wiring import Component, In, Out

from .grammar import GpReg
from .init import GpRegInit


class GpRegFile(Component):

    read_gp_reg_a: In(GpReg, init = GpReg.R0)
    read_gp_reg_b: In(GpReg, init = GpReg.R0)
    write_gp_reg: In(GpReg, init = GpReg.R0)
    write_data: In(8)
    write_enable: In(1)

    read_data_a: Out(8)
    read_data_b: Out(8)


    def __init__(self, init_data: GpRegInit = None):
        init_data = init_data or GpRegInit()
        self.gp_regs: Array = Array([
            Signal(8, init = reg_data) for reg_data in [0x00, *astuple(init_data)]
        ])
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        with m.If(self.read_gp_reg_a == GpReg.R0):
            m.d.comb += self.read_data_a.eq(0x00)
        with m.Else():
            m.d.comb += self.read_data_a.eq(self.gp_regs[self.read_gp_reg_a])

        with m.If(self.read_gp_reg_b == GpReg.R0):
            m.d.comb += self.read_data_b.eq(0x00)
        with m.Else():
            m.d.comb += self.read_data_b.eq(self.gp_regs[self.read_gp_reg_b])

        with m.If(self.write_enable & (self.write_gp_reg != GpReg.R0)):
            m.d.sync += self.gp_regs[self.write_gp_reg].eq(self.write_data)

        return m
