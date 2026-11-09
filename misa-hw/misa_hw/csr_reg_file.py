from amaranth import Cat, Module, Signal
from amaranth.lib.wiring import Component, In, Out

from .grammar import CsrReg, Extn
from .init import CsrRegInit


class CsrRegFile(Component):

    read_csr_reg: In(CsrReg, init = CsrReg.SADDR)
    write_csr_reg: In(CsrReg, init = CsrReg.SADDR)
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
        self.fault: Signal = Signal(16, init = init_data.fault)
        self.retsc: Signal = Signal(16, init = init_data.retsc)
        self.retir: Signal = Signal(16, init = init_data.retir)
        self.privs: Signal = Signal(1, init = init_data.privs)
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        with m.Switch(self.read_csr_reg):
            with m.Case(CsrReg.SADDR):
                m.d.comb += self.read_data.eq(self.saddr)
            with m.Case(CsrReg.RADDR):
                m.d.comb += self.read_data.eq(self.raddr)
            with m.Case(CsrReg.FLAGS):
                m.d.comb += self.read_data.eq(self.flags)
            with m.Case(CsrReg.CAUSE):
                m.d.comb += self.read_data.eq(self.cause)
            with m.Case(CsrReg.EXTNS):
                m.d.comb += self.read_data.eq(self.extns)
            with m.Case(CsrReg.FAULT):
                m.d.comb += self.read_data.eq(self.fault)
            with m.Case(CsrReg.RETSC):
                m.d.comb += self.read_data.eq(self.retsc)
            with m.Case(CsrReg.RETIR):
                m.d.comb += self.read_data.eq(self.retir)
            with m.Case(CsrReg.PRIVS):
                m.d.comb += self.read_data.eq(self.privs)

        with m.If(self.write_enable):
            with m.Switch(self.write_csr_reg):
                with m.Case(CsrReg.SADDR):
                    m.d.sync += self.saddr.eq(self.write_data)
                with m.Case(CsrReg.RADDR):
                    m.d.sync += self.raddr.eq(self.write_data)
                with m.Case(CsrReg.FLAGS):
                    m.d.sync += self.flags.eq(self.write_data)
                with m.Case(CsrReg.CAUSE):
                    m.d.sync += self.cause.eq(self.write_data)
                with m.Case(CsrReg.EXTNS):
                    with m.If(self.extns & Extn.DYNAMICHW):
                        m.d.sync += self.extns.eq(Cat(0b1, self.write_data[1:4]))
                with m.Case(CsrReg.FAULT):
                    m.d.sync += self.fault.eq(self.write_data)
                with m.Case(CsrReg.RETSC):
                    m.d.sync += self.retsc.eq(self.write_data)
                with m.Case(CsrReg.RETIR):
                    m.d.sync += self.retir.eq(self.write_data)
                with m.Case(CsrReg.PRIVS):
                    m.d.sync += self.privs.eq(self.write_data)

        m.d.comb += [
            self.flags_data.eq(self.flags),
            self.extns_data.eq(self.extns),
            self.privs_data.eq(self.privs)
        ]

        return m
