from amaranth import Cat, Module, Signal, Value
from amaranth.lib.wiring import Component, In, Out

from .grammar import Csr, Extn
from .init import CsrInit


class CsrFile(Component):

    read_reg: In(Csr, init = Csr.SADDR)
    write_reg: In(Csr, init = Csr.SADDR)
    write_data: In(16)
    write_enable: In(1)
    flags_write_enable: In(1)
    flags_write_data: In(4)

    read_data: Out(16)
    flags_data: Out(4)
    extns_data: Out(4)
    privs_data: Out(1)


    def __init__(self, init_data: CsrInit = None) -> None:
        if (init_data is None):
            init_data = CsrInit()
        self.saddr: Signal = Signal(16, init = init_data.saddr)
        self.raddr: Signal = Signal(16, init = init_data.raddr)
        self.flags: Signal = Signal(4,  init = init_data.flags)
        self.cause: Signal = Signal(16, init = init_data.cause)
        self.extns: Signal = Signal(4,  init = init_data.extns)
        self.retsc: Signal = Signal(16, init = init_data.retsc)
        self.retir: Signal = Signal(16, init = init_data.retir)
        self.privs: Signal = Signal(1,  init = init_data.privs)
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        m.d.comb += [
            self.flags_data.eq(self.flags),
            self.extns_data.eq(self.extns),
            self.privs_data.eq(self.privs)
        ]

        with m.Switch(self.read_reg):
            with m.Case(Csr.SADDR):
                m.d.comb += self.read_data.eq(self.saddr)
            with m.Case(Csr.RADDR):
                m.d.comb += self.read_data.eq(self.raddr)
            with m.Case(Csr.FLAGS):
                m.d.comb += self.read_data.eq(self.flags)
            with m.Case(Csr.CAUSE):
                m.d.comb += self.read_data.eq(self.cause)
            with m.Case(Csr.EXTNS):
                m.d.comb += self.read_data.eq(self.extns)
            with m.Case(Csr.RETSC):
                m.d.comb += self.read_data.eq(self.retsc)
            with m.Case(Csr.RETIR):
                m.d.comb += self.read_data.eq(self.retir)
            with m.Case(Csr.PRIVS):
                m.d.comb += self.read_data.eq(self.privs)

        with m.If(self.write_enable):
            with m.Switch(self.write_reg):
                with m.Case(Csr.SADDR):
                    m.d.sync += self.saddr.eq(self.write_data)
                with m.Case(Csr.RADDR):
                    m.d.sync += self.raddr.eq(self.write_data)
                with m.Case(Csr.FLAGS):
                    m.d.sync += self.flags.eq(self.write_data[0:4])
                with m.Case(Csr.CAUSE):
                    m.d.sync += self.cause.eq(self.write_data)
                with m.Case(Csr.EXTNS):
                    with m.If(self.extns & Extn.DYNAMICHW):
                        m.d.sync += self.extns.eq((self.write_data[0:4] & 0b1110) | Extn.DYNAMICHW)
                with m.Case(Csr.RETSC):
                    m.d.sync += self.retsc.eq(self.write_data)
                with m.Case(Csr.RETIR):
                    m.d.sync += self.retir.eq(self.write_data)
                with m.Case(Csr.PRIVS):
                    m.d.sync += self.privs.eq(self.write_data[0])

        with m.If(self.flags_write_enable):
            m.d.sync += self.flags.eq(self.flags_write_data)

        return m
