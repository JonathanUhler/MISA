from enum import StrEnum

from amaranth import Cat, Module, Signal, Value
from amaranth.lib.enum import IntEnum
from amaranth.lib.memory import Memory, ReadPort, WritePort
from amaranth.lib.wiring import Component, In, Out


class QspiCmd(IntEnum, shape = 4):
    READ  = 0x3
    WRITE = 0x2


class QspiState(StrEnum):
    IDLE = "IDLE"
    XFER = "XFER"
    DONE = "DONE"


class QspiController(Component):

    mem_addr: In(16)
    mem_write_data: In(8)
    mem_write_enable: In(1)
    mem_request: In(1)
    mem_read_data: Out(8)
    mem_ready: Out(1)

    sck: Out(1)
    cs0: Out(1)
    cs1: Out(1)
    cs2: Out(1)
    sd_o: Out(4)
    sd_oe: Out(4)
    sd_i: In(4)


    def __init__(self, flash_base: int = 0x8000) -> None:
        self.flash_base: int = flash_base
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        addr_q: Signal = Signal(16)
        wdata_q: Signal = Signal(8)
        we_q: Signal = Signal(1)
        rdata_q: Signal = Signal(8)
        cnt: Signal = Signal(4)
        to_flash: Signal = Signal(1)

        m.d.comb += [
            self.cs0.eq(1),
            self.cs1.eq(1),
            self.cs2.eq(1),
            self.sck.eq(0),
            self.sd_o.eq(0),
            self.sd_oe.eq(0),
            self.mem_ready.eq(0),
            self.mem_read_data.eq(rdata_q)
        ]

        addr_nibble: Signal = Signal(4)
        with m.Switch(cnt):
            with m.Case(1):
                m.d.comb += addr_nibble.eq(addr_q[12 : 16])
            with m.Case(2):
                m.d.comb += addr_nibble.eq(addr_q[8 : 12])
            with m.Case(3):
                m.d.comb += addr_nibble.eq(addr_q[4 : 8])
            with m.Case(4):
                m.d.comb += addr_nibble.eq(addr_q[0 : 4])

        with m.FSM(init = QspiState.IDLE):
            with m.State(QspiState.IDLE):
                with m.If(self.mem_request):
                    m.d.sync += [
                        addr_q.eq(self.mem_addr),
                        wdata_q.eq(self.mem_write_data),
                        we_q.eq(self.mem_write_enable),
                        to_flash.eq(self.mem_addr >= self.flash_base),
                        cnt.eq(0)
                    ]
                    m.next = QspiState.XFER

            with m.State(QspiState.XFER):
                m.d.comb += [
                    self.cs0.eq(~to_flash),
                    self.cs1.eq(to_flash),
                    self.cs2.eq(1),
                    self.sck.eq(cnt[0])
                ]
                with m.If(cnt == 0):
                    m.d.comb += [self.sd_o.eq(QspiCmd.WRITE), self.sd_oe.eq(0xF)]
                    with m.If(~we_q):
                        m.d.comb += self.sd_o.eq(QspiCmd.READ)
                with m.Elif((cnt >= 1) & (cnt <= 4)):
                    m.d.comb += [self.sd_o.eq(addr_nibble), self.sd_oe.eq(0xF)]
                with m.Elif(we_q & (cnt == 5)):
                    m.d.comb += [self.sd_o.eq(wdata_q[4 : 8]), self.sd_oe.eq(0xF)]
                with m.Elif(we_q & (cnt == 6)):
                    m.d.comb += [self.sd_o.eq(wdata_q[0 : 4]), self.sd_oe.eq(0xF)]
                with m.If(~we_q & (cnt == 6)):
                    m.d.sync += rdata_q[4 : 8].eq(self.sd_i)
                with m.If(~we_q & (cnt == 7)):
                    m.d.sync += rdata_q[0 : 4].eq(self.sd_i)

                m.d.sync += cnt.eq(cnt + 1)
                with m.If(we_q & (cnt == 6)):
                    m.next = QspiState.DONE
                with m.If(~we_q & (cnt == 7)):
                    m.next = QspiState.DONE

            with m.State(QspiState.DONE):
                m.d.comb += self.mem_ready.eq(1)
                m.next = QspiState.IDLE

        return m


class QspiMemModel(Component):

    sck: In(1)
    cs0: In(1)
    cs1: In(1)
    cs2: In(1)
    sd_i: In(4)
    sd_o: Out(4)
    sd_oe: Out(4)


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

        selected: Value = ~self.cs0 | ~self.cs1 | ~self.cs2
        cnt: Signal = Signal(4)
        cmd: Signal = Signal(4)
        addr: Signal = Signal(16)
        wbyte: Signal = Signal(8)

        m.d.comb += [
            self.sd_o.eq(0),
            self.sd_oe.eq(0),
            rp.addr.eq(addr),
            wp.addr.eq(addr),
            wp.data.eq(wbyte),
            wp.en.eq(0)
        ]

        reading: Value = cmd == QspiCmd.READ
        with m.If(~selected):
            m.d.sync += cnt.eq(0)
        with m.Else():
            m.d.sync += cnt.eq(cnt + 1)
            with m.Switch(cnt):
                with m.Case(0):
                    m.d.sync += cmd.eq(self.sd_i)
                with m.Case(1):
                    m.d.sync += addr[12 : 16].eq(self.sd_i)
                with m.Case(2):
                    m.d.sync += addr[8 : 12].eq(self.sd_i)
                with m.Case(3):
                    m.d.sync += addr[4 : 8].eq(self.sd_i)
                with m.Case(4):
                    m.d.sync += addr[0 : 4].eq(self.sd_i)
                with m.Case(5):
                    with m.If(~reading):
                        m.d.sync += wbyte[4 : 8].eq(self.sd_i)
            with m.If(~reading & (cnt == 6)):
                m.d.comb += [wp.en.eq(1), wp.data.eq(Cat(self.sd_i, wbyte[4 : 8]))]
            with m.If(reading & (cnt == 6)):
                m.d.comb += [self.sd_o.eq(rp.data[4 : 8]), self.sd_oe.eq(0xF)]
            with m.If(reading & (cnt == 7)):
                m.d.comb += [self.sd_o.eq(rp.data[0 : 4]), self.sd_oe.eq(0xF)]

        return m


def connect_qspi(m: Module, ctrl: QspiController, model: QspiMemModel) -> None:
    bus: Signal = Signal(4)
    m.d.comb += [
        model.cs0.eq(ctrl.cs0),
        model.cs1.eq(ctrl.cs1),
        model.cs2.eq(ctrl.cs2),
        model.sck.eq(ctrl.sck)
    ]
    with m.If(ctrl.sd_oe.any()):
        m.d.comb += bus.eq(ctrl.sd_o)
    with m.Elif(model.sd_oe.any()):
        m.d.comb += bus.eq(model.sd_o)
    m.d.comb += [ctrl.sd_i.eq(bus), model.sd_i.eq(bus)]
