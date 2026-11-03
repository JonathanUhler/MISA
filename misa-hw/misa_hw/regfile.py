"""The sixteen 8-bit general-purpose registers, with R0 hardwired to zero.

One combinational read port and one synchronous write port; the multi-cycle core sequences operand
accesses so a single port of each suffices.
"""

from amaranth import Module, Signal, Array
from amaranth.lib import wiring
from amaranth.lib.wiring import In, Out


class RegFile(wiring.Component):
    raddr: In(4)
    rdata: Out(8)
    waddr: In(4)
    wdata: In(8)
    we: In(1)

    def __init__(self, init: list = None) -> None:
        init: list = list(init) if init is not None else [0] * 16
        # Signals are created here (not in elaborate) so a test harness can read them back.
        self.regs: list = [Signal(8, init=init[i], name=f"gpr{i}") for i in range(16)]
        super().__init__()

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()
        regs: Array = Array(self.regs)

        with m.If(self.raddr == 0):
            m.d.comb += self.rdata.eq(0)
        with m.Else():
            m.d.comb += self.rdata.eq(regs[self.raddr])

        with m.If(self.we & (self.waddr != 0)):
            m.d.sync += regs[self.waddr].eq(self.wdata)

        return m
