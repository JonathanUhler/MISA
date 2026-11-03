"""The single-byte memory-port contract and a simulation-only idealized memory.

The memory-port contract is the interface every memory implementation presents to the core:

    mem_addr  : In(16)   address of the transaction
    mem_wdata : In(8)    write data
    mem_we    : In(1)    1 = write, 0 = read
    mem_req   : In(1)    pulse high for one cycle to start a transaction
    mem_rdata : Out(8)   read data, valid the cycle mem_ready is high
    mem_ready : Out(1)   high for one cycle when the transaction completes

`IdealMemory` is a 64 KiB implementation for simulation only. It is never synthesized or taped out;
it exists so the core can be verified before the QSPI controller (Task 19) exists.
"""

from enum import auto

from amaranth import Module, Signal
from amaranth.lib import wiring, memory
from amaranth.lib.wiring import In, Out

from .config import StateEnum


class MemState(StateEnum):
    """IdealMemory request/response FSM states."""
    IDLE = auto()
    DONE = auto()


class IdealMemory(wiring.Component):
    mem_addr: In(16)
    mem_wdata: In(8)
    mem_we: In(1)
    mem_req: In(1)
    mem_rdata: Out(8)
    mem_ready: Out(1)

    def __init__(self, init: list = None) -> None:
        init: list = list(init or [])
        if len(init) < 65536:
            init: list = init + [0] * (65536 - len(init))
        self._init: list = init
        super().__init__()

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()
        mem: memory.Memory = memory.Memory(shape=8, depth=65536, init=self._init)
        m.submodules.mem = mem
        rp: memory.ReadPort = mem.read_port(domain="comb")
        wp: memory.WritePort = mem.write_port()

        addr_q: Signal = Signal(16)
        wdata_q: Signal = Signal(8)
        we_q: Signal = Signal()

        m.d.comb += [
            rp.addr.eq(addr_q),
            self.mem_rdata.eq(rp.data),
            wp.addr.eq(addr_q),
            wp.data.eq(wdata_q),
            wp.en.eq(0),
        ]

        with m.FSM(init=MemState.IDLE):
            with m.State(MemState.IDLE):
                with m.If(self.mem_req):
                    m.d.sync += [
                        addr_q.eq(self.mem_addr),
                        wdata_q.eq(self.mem_wdata),
                        we_q.eq(self.mem_we),
                    ]
                    m.next = MemState.DONE
            with m.State(MemState.DONE):
                # Read data is combinational from addr_q; a write commits on the edge out of DONE.
                m.d.comb += [self.mem_ready.eq(1), wp.en.eq(we_q)]
                m.next = MemState.IDLE

        return m
