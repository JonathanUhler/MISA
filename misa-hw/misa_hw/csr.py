"""The special (control and status) register file.

Holds the architectural CSRs and implements the FLAGS and EXTNS write special-cases from misa-spec's
`writeSpecial`. Context state (inSyscall, inInterrupt, halted, and the interrupt/syscall shadows)
lives in the control FSM, not here.
"""

from amaranth import Module, Signal
from amaranth.lib import wiring
from amaranth.lib.wiring import In, Out

from .types import Csr, EXT_DYNAMICHW
from .config import SpecInit


class SpecFile(wiring.Component):
    raddr: In(Csr, init=Csr.SADDR)
    rdata: Out(16)
    waddr: In(Csr, init=Csr.SADDR)
    wdata: In(16)
    we: In(1)
    flags_we: In(1)
    flags_in: In(4)
    flags_out: Out(4)
    extns_out: Out(4)
    privs_out: Out(1)

    def __init__(self, extns_reset: int = 0x0F, init: SpecInit = None) -> None:
        # init: optional SpecInit for an arbitrary-state start (extns_reset supplies EXTNS when init
        # is None). Signals live here so a harness can read them back.
        s: SpecInit = init if init is not None else SpecInit()
        extns_init: int = init.extns if init is not None else extns_reset
        self.saddr: Signal = Signal(16, init=s.saddr)
        self.raddr_r: Signal = Signal(16, init=s.raddr)
        self.cause: Signal = Signal(16, init=s.cause)
        self.retsc: Signal = Signal(16, init=s.retsc)
        self.retir: Signal = Signal(16, init=s.retir)
        self.flags: Signal = Signal(4, init=s.flags & 0xF)
        self.extns: Signal = Signal(4, init=extns_init & 0xF)
        self.privs: Signal = Signal(1, init=s.privs & 0x1)
        super().__init__()

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        saddr: Signal = self.saddr
        raddr_r: Signal = self.raddr_r
        cause: Signal = self.cause
        retsc: Signal = self.retsc
        retir: Signal = self.retir
        flags: Signal = self.flags
        extns: Signal = self.extns
        privs: Signal = self.privs

        m.d.comb += [
            self.flags_out.eq(flags),
            self.extns_out.eq(extns),
            self.privs_out.eq(privs),
        ]

        with m.Switch(self.raddr):
            with m.Case(Csr.SADDR):
                m.d.comb += self.rdata.eq(saddr)
            with m.Case(Csr.RADDR):
                m.d.comb += self.rdata.eq(raddr_r)
            with m.Case(Csr.FLAGS):
                m.d.comb += self.rdata.eq(flags)
            with m.Case(Csr.CAUSE):
                m.d.comb += self.rdata.eq(cause)
            with m.Case(Csr.EXTNS):
                m.d.comb += self.rdata.eq(extns)
            with m.Case(Csr.RETSC):
                m.d.comb += self.rdata.eq(retsc)
            with m.Case(Csr.RETIR):
                m.d.comb += self.rdata.eq(retir)
            with m.Case(Csr.PRIVS):
                m.d.comb += self.rdata.eq(privs)

        with m.If(self.we):
            with m.Switch(self.waddr):
                with m.Case(Csr.SADDR):
                    m.d.sync += saddr.eq(self.wdata)
                with m.Case(Csr.RADDR):
                    m.d.sync += raddr_r.eq(self.wdata)
                with m.Case(Csr.FLAGS):
                    m.d.sync += flags.eq(self.wdata[0:4])
                with m.Case(Csr.CAUSE):
                    m.d.sync += cause.eq(self.wdata)
                with m.Case(Csr.EXTNS):
                    # Writable only when Dynamic Hardware is live; bit 0 is pinned.
                    with m.If(extns[EXT_DYNAMICHW]):
                        m.d.sync += extns.eq((self.wdata[0:4] & 0b1110) | (extns & 0b0001))
                with m.Case(Csr.RETSC):
                    m.d.sync += retsc.eq(self.wdata)
                with m.Case(Csr.RETIR):
                    m.d.sync += retir.eq(self.wdata)
                with m.Case(Csr.PRIVS):
                    m.d.sync += privs.eq(self.wdata[0])

        # A dedicated flag update (from the ALU path) wins over a general write to FLAGS.
        with m.If(self.flags_we):
            m.d.sync += flags.eq(self.flags_in)

        return m
