from enum import StrEnum

from amaranth import Module, Signal
from amaranth.lib.memory import Memory, ReadPort, WritePort
from amaranth.lib.wiring import Component, In, Out


class MemState(StrEnum):
    IDLE = "IDLE"
    DONE = "DONE"


class IdealMemory(Component):

    mem_addr: In(16)
    mem_write_data: In(8)
    mem_write_enable: In(1)
    mem_request: In(1)
    mem_read_data: Out(8)
    mem_ready: Out(1)


    def __init__(self, init: list = None) -> None:
        init = list(init or [])
        if (len(init) < 65536):
            init = init + [0] * (65536 - len(init))
        self._init: list = init
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        mem: Memory = Memory(shape = 8, depth = 65536, init = self._init)
        m.submodules.mem = mem
        rp: ReadPort = mem.read_port(domain = "comb")
        wp: WritePort = mem.write_port()

        addr_q: Signal = Signal(16)
        wdata_q: Signal = Signal(8)
        we_q: Signal = Signal(1)

        m.d.comb += [
            rp.addr.eq(addr_q),
            self.mem_read_data.eq(rp.data),
            wp.addr.eq(addr_q),
            wp.data.eq(wdata_q),
            wp.en.eq(0)
        ]

        with m.FSM(init = MemState.IDLE):
            with m.State(MemState.IDLE):
                with m.If(self.mem_request):
                    m.d.sync += [
                        addr_q.eq(self.mem_addr),
                        wdata_q.eq(self.mem_write_data),
                        we_q.eq(self.mem_write_enable)
                    ]
                    m.next = MemState.DONE
            with m.State(MemState.DONE):
                m.d.comb += [self.mem_ready.eq(1), wp.en.eq(we_q)]
                m.next = MemState.IDLE

        return m
