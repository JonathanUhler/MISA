from dataclasses import astuple

from amaranth import Array, Cat, Module, Signal
from amaranth.lib.wiring import Component, In, Out

from .grammar import CsrReg, Extn
from .init import RegInit


class CsrRegFile(Component):

    read_csr_reg: In(GpReg)
    write_csr_reg: In(GpReg)
    write_data: In(16)
    write_enable: In(1)

    read_data: Out(16)
    flags_data: Out(4)
    extns_data: Out(4)
    privs_data: Out(1)


    def __init__(self, init_data: CsrRegInit = None):
        init_data = init_data or CsrRegInit()
        self.saddr: Signal = Signal(16, init = init_data.saddr)
        self.raddr: Signal = Signal(16, init = init_data.raddr)
        self.flags: Signal = Signal(4, init = init_data.flags)
        self.cause: Signal = Signal(16, init = init_data.cause)
        self.extns: Signal = Signal(4, init = init_data.extns)
        self.retsc: Signal = Signal(16, init = init_data.retsc)
        self.retir: Signal = Signal(16, init = init_data.retir)
        self.privs: Signal = Signal(1, init = init_data.privs)
        self.csr_regs: Array = Array([
            None, self.saddr, self.raddr, self.flags, self.cause, self.extns,
            self.retsc, self.retir, self.privs
        ])
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        m.d.comb += self.read_data.eq(self.csr_regs[self.read_csr_reg])

        with m.If(self.write_enable):
            with m.If(self.write_csr_reg == CsrReg.EXTNS):
                with m.If(self.extns & Extn.DYNAMICHW):
                    m.d.sync += self.extns.eq(Cat(0b1, self.write_data[1:4]))
            with m.Else():
                m.d.sync += self.csr_regs[self.write_csr_reg].eq(self.write_data)

        m.d.comb += [
            self.flags_data.eq(self.flags),
            self.extns_data.eq(self.extns),
            self.privs_data.eq(self.privs)
        ]

        return m
