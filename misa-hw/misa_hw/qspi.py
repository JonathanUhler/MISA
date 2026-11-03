"""Serial QSPI memory controller and a behavioral flash+PSRAM model for simulation.

`QspiController` presents the single-byte memory-port contract (see memory.py) to the core and acts
as a QSPI master on the pins: it frames each transaction with a chip select, shifts a command nibble
and a 16-bit address out over the four data lines (most-significant nibble first), then shifts a data
byte out (write) or in (read). It is fully synchronous and derives the serial clock from the system
clock, so it behaves identically when the system clock is slowed or single-stepped.

`QspiMemModel` is a behavioral QSPI slave backing the whole 64 KiB space, for simulation only. It is
cycle-accurate against the controller on the shared system clock rather than sampling on the SCK
edge, which is sufficient to verify the controller's framing and the core end-to-end; a real device
in the SCK domain is validated on the carrier board after manufacture.

Transaction schedule (cycles counted from chip-select assertion, 4 bits per cycle):

    cnt 0        command nibble   (READ=0x3, WRITE=0x2)
    cnt 1..4     address, nibble [15:12],[11:8],[7:4],[3:0]
    write  cnt 5 data[7:4], cnt 6 data[3:0]
    read   cnt 5 bus turnaround,  cnt 6 data[7:4], cnt 7 data[3:0]
"""

from enum import auto

from amaranth import Module, Signal, Cat, Value
from amaranth.lib import wiring, memory, enum

from amaranth.lib.wiring import In, Out

from .config import StateEnum


class QspiCmd(enum.IntEnum, shape=4):
    """Command nibble placed on the data lines at the start of a transaction."""
    READ = 0x3
    WRITE = 0x2


class QspiState(StateEnum):
    """QspiController master FSM states."""
    IDLE = auto()
    XFER = auto()
    DONE = auto()


class QspiController(wiring.Component):
    # Memory-port slave side (to the core).
    mem_addr: In(16)
    mem_wdata: In(8)
    mem_we: In(1)
    mem_req: In(1)
    mem_rdata: Out(8)
    mem_ready: Out(1)
    # QSPI master pins.
    sck: Out(1)
    cs0: Out(1)   # flash   (active low)
    cs1: Out(1)   # psram A (active low)
    cs2: Out(1)   # psram B (active low)
    sd_o: Out(4)
    sd_oe: Out(4)
    sd_i: In(4)

    def __init__(self, flash_base: int = 0x8000) -> None:
        self._flash_base: int = flash_base
        super().__init__()

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        addr_q: Signal = Signal(16)
        wdata_q: Signal = Signal(8)
        we_q: Signal = Signal()
        rdata_q: Signal = Signal(8)
        cnt: Signal = Signal(4)
        to_flash: Signal = Signal()

        # Idle levels: all chip selects deasserted (high), bus released, clock low.
        m.d.comb += [self.cs0.eq(1), self.cs1.eq(1), self.cs2.eq(1),
                     self.sck.eq(0), self.sd_o.eq(0), self.sd_oe.eq(0),
                     self.mem_ready.eq(0), self.mem_rdata.eq(rdata_q)]

        addr_nibble: Signal = Signal(4)
        with m.Switch(cnt):
            with m.Case(1):
                m.d.comb += addr_nibble.eq(addr_q[12:16])
            with m.Case(2):
                m.d.comb += addr_nibble.eq(addr_q[8:12])
            with m.Case(3):
                m.d.comb += addr_nibble.eq(addr_q[4:8])
            with m.Case(4):
                m.d.comb += addr_nibble.eq(addr_q[0:4])

        with m.FSM(init=QspiState.IDLE):
            with m.State(QspiState.IDLE):
                with m.If(self.mem_req):
                    m.d.sync += [
                        addr_q.eq(self.mem_addr),
                        wdata_q.eq(self.mem_wdata),
                        we_q.eq(self.mem_we),
                        to_flash.eq(self.mem_addr >= self._flash_base),
                        cnt.eq(0),
                    ]
                    m.next = QspiState.XFER

            with m.State(QspiState.XFER):
                # Chip select active (low) for the selected device; SCK toggles while active.
                m.d.comb += [self.cs0.eq(~to_flash), self.cs1.eq(to_flash), self.cs2.eq(1),
                             self.sck.eq(cnt[0])]
                # Drive command / address / (write) data out; release the bus during read data.
                with m.If(cnt == 0):
                    m.d.comb += [self.sd_o.eq(QspiCmd.WRITE), self.sd_oe.eq(0xF)]
                    with m.If(~we_q):
                        m.d.comb += self.sd_o.eq(QspiCmd.READ)
                with m.Elif((cnt >= 1) & (cnt <= 4)):
                    m.d.comb += [self.sd_o.eq(addr_nibble), self.sd_oe.eq(0xF)]
                with m.Elif(we_q & (cnt == 5)):
                    m.d.comb += [self.sd_o.eq(wdata_q[4:8]), self.sd_oe.eq(0xF)]
                with m.Elif(we_q & (cnt == 6)):
                    m.d.comb += [self.sd_o.eq(wdata_q[0:4]), self.sd_oe.eq(0xF)]
                # Read: sample the slave's nibbles at cnt 6 and 7.
                with m.If(~we_q & (cnt == 6)):
                    m.d.sync += rdata_q[4:8].eq(self.sd_i)
                with m.If(~we_q & (cnt == 7)):
                    m.d.sync += rdata_q[0:4].eq(self.sd_i)

                m.d.sync += cnt.eq(cnt + 1)
                with m.If(we_q & (cnt == 6)):
                    m.next = QspiState.DONE
                with m.If(~we_q & (cnt == 7)):
                    m.next = QspiState.DONE

            with m.State(QspiState.DONE):
                m.d.comb += self.mem_ready.eq(1)
                m.next = QspiState.IDLE

        return m


class QspiMemModel(wiring.Component):
    """Behavioral QSPI slave backing a full 64 KiB space (simulation only)."""
    sck: In(1)
    cs0: In(1)
    cs1: In(1)
    cs2: In(1)
    sd_i: In(4)
    sd_o: Out(4)
    sd_oe: Out(4)

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

        selected: Value = ~self.cs0 | ~self.cs1 | ~self.cs2
        cnt: Signal = Signal(4)
        cmd: Signal = Signal(4)
        addr: Signal = Signal(16)
        wbyte: Signal = Signal(8)

        m.d.comb += [self.sd_o.eq(0), self.sd_oe.eq(0), rp.addr.eq(addr),
                     wp.addr.eq(addr), wp.data.eq(wbyte), wp.en.eq(0)]

        reading: Value = cmd == QspiCmd.READ
        with m.If(~selected):
            m.d.sync += cnt.eq(0)
        with m.Else():
            m.d.sync += cnt.eq(cnt + 1)
            with m.Switch(cnt):
                with m.Case(0):
                    m.d.sync += cmd.eq(self.sd_i)
                with m.Case(1):
                    m.d.sync += addr[12:16].eq(self.sd_i)
                with m.Case(2):
                    m.d.sync += addr[8:12].eq(self.sd_i)
                with m.Case(3):
                    m.d.sync += addr[4:8].eq(self.sd_i)
                with m.Case(4):
                    m.d.sync += addr[0:4].eq(self.sd_i)
                with m.Case(5):
                    with m.If(~reading):
                        m.d.sync += wbyte[4:8].eq(self.sd_i)
            # Write: commit the byte on the cnt 6 edge (low nibble arrives this cycle).
            with m.If(~reading & (cnt == 6)):
                m.d.comb += [wp.en.eq(1), wp.data.eq(Cat(self.sd_i, wbyte[4:8]))]
            # Read: drive the data nibbles out on cnt 6 and 7.
            with m.If(reading & (cnt == 6)):
                m.d.comb += [self.sd_o.eq(rp.data[4:8]), self.sd_oe.eq(0xF)]
            with m.If(reading & (cnt == 7)):
                m.d.comb += [self.sd_o.eq(rp.data[0:4]), self.sd_oe.eq(0xF)]

        return m


def connect_qspi(m: Module, ctrl: QspiController, model: QspiMemModel) -> None:
    """Wire a controller to a model: chip selects, clock, and the shared tri-state data bus."""
    bus: Signal = Signal(4)
    m.d.comb += [
        model.cs0.eq(ctrl.cs0), model.cs1.eq(ctrl.cs1), model.cs2.eq(ctrl.cs2),
        model.sck.eq(ctrl.sck),
    ]
    with m.If(ctrl.sd_oe.any()):
        m.d.comb += bus.eq(ctrl.sd_o)
    with m.Elif(model.sd_oe.any()):
        m.d.comb += bus.eq(model.sd_o)
    m.d.comb += [ctrl.sd_i.eq(bus), model.sd_i.eq(bus)]
