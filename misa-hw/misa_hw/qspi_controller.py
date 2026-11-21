from enum import StrEnum

from amaranth import C, Cat, Module, Mux, Signal, Value
from amaranth.lib.enum import IntEnum
from amaranth.lib.wiring import Component, In, Out

from .grammar import AccessKind


class QspiCmd(IntEnum, shape = 8):
    WRITE      = 0x02
    FAST_READ  = 0x0B
    ENTER_QUAD = 0x35
    RESET_EN   = 0x66
    RESET      = 0x99
    READ_ID    = 0x9F


class ChipSelect(IntEnum, shape = 2):
    FLASH = 0
    RAM_A = 1
    RAM_B = 2


class InitStep(IntEnum, shape = 2):
    RESET_EN = 0
    RESET    = 1
    PROBE    = 2
    QUAD     = 3


class QspiState(StrEnum):
    POWER_UP      = "POWER_UP"
    INIT_SETUP    = "INIT_SETUP"
    INIT_SHIFT    = "INIT_SHIFT"
    INIT_IDENTITY = "INIT_IDENTITY"
    INIT_GAP      = "INIT_GAP"
    DEAD_BUS      = "DEAD_BUS"

    IDLE       = "IDLE"
    CLOSE      = "CLOSE"
    CMD        = "CMD"
    ADDR       = "ADDR"
    WAIT       = "WAIT"
    READ_DATA  = "READ_DATA"
    LOOKAHEAD  = "LOOKAHEAD"
    WRITE_DATA = "WRITE_DATA"


CMD_PERIODS: int = 2
ADDR_PERIODS: int = 6
BYTE_PERIODS: int = 2
WAIT_PERIODS: int = 4

PROBE_CMD_BITS: int = 8 + 24
IDENTITY_BITS: int = 8
PROBE_PERIODS: int = PROBE_CMD_BITS + IDENTITY_BITS

PAGE_BITS: int = 10

TCEM_MARGIN: int = 4

SD_OE_QUAD: int = 0b1111
SD_OE_SINGLE: int = 0b1101
SD_OE_NONE: int = 0b0000


class QspiController(Component):

    # MisaCore connections
    mem_addr: In(16)
    mem_write_data: In(8)
    mem_write_enable: In(1)
    mem_request: In(1)

    mem_ready: Out(1)
    mem_read_data: Out(8)
    mem_mapped: Out(1)
    mem_caps: Out(3)

    # Memory device connections
    sck: Out(1)
    cs_n: Out(3)
    sd_o: Out(4)
    sd_oe: Out(4)
    sd_i: In(4)
    bus_claim: Out(1)

    # Diagnostics for TtUmMisa uo_out
    device_id: Out(8)
    device_found: Out(1)


    def __init__(self, power_up_cycles: int = 4800, tcem_periods: int = 48, psram_id: int = 0x0D):
        if (tcem_periods < PROBE_PERIODS):
            raise ValueError(
                f"tcem_periods={tcem_periods} cannot hold the {PROBE_PERIODS} period identity"
                f"probe, so SCK is too slow for this part: raise the clock or shorten the probe"
            )
        self.power_up_cycles: int = power_up_cycles
        self.tcem_periods: int = tcem_periods
        self.psram_id: int = psram_id
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        self.elaborate_clocking(m)
        self.elaborate_datapath(m)
        self.elaborate_pins(m)

        states: list = [
            (QspiState.POWER_UP,      self.elaborate_power_up),
            (QspiState.INIT_SETUP,    self.elaborate_init_setup),
            (QspiState.INIT_SHIFT,    self.elaborate_init_shift),
            (QspiState.INIT_IDENTITY, self.elaborate_init_identity),
            (QspiState.INIT_GAP,      self.elaborate_init_gap),
            (QspiState.DEAD_BUS,      self.elaborate_dead_bus),
            (QspiState.IDLE,          self.elaborate_idle),
            (QspiState.CLOSE,         self.elaborate_close),
            (QspiState.CMD,           self.elaborate_cmd),
            (QspiState.ADDR,          self.elaborate_addr),
            (QspiState.WAIT,          self.elaborate_wait),
            (QspiState.READ_DATA,     self.elaborate_read_data),
            (QspiState.LOOKAHEAD,     self.elaborate_lookahead),
            (QspiState.WRITE_DATA,    self.elaborate_write_data)
        ]

        with m.FSM(init = QspiState.POWER_UP):
            for state, elaborator in states:
                with m.State(state):
                    elaborator(m)

        return m


    def elaborate_clocking(self, m: Module) -> None:
        self.phase: Signal = Signal(1)
        self.tick: Signal = Signal(1)
        self.clocking: Signal = Signal(1)
        self.sample: Signal = Signal(4)

        m.d.sync += self.phase.eq(~self.phase)
        m.d.comb += self.tick.eq(self.phase)

        with m.If(~self.tick):
            m.d.sync += self.sample.eq(self.sd_i)

        m.d.comb += self.sck.eq(self.phase & self.clocking)


    def elaborate_datapath(self, m: Module) -> None:
        self.shift_out: Signal = Signal(32)
        self.shift_in: Signal = Signal(8)
        self.count: Signal = Signal(range(PROBE_CMD_BITS + 1))
        self.quad: Signal = Signal(1)
        self.cs_assert: Signal = Signal(1)
        self.selected: Signal = Signal(ChipSelect, init = ChipSelect.RAM_A)
        self.init_step: Signal = Signal(InitStep)
        self.startup: Signal = Signal(range(self.power_up_cycles + 1))
        self.writing: Signal = Signal(1)

        self.burst_open: Signal = Signal(1)
        self.burst_next: Signal = Signal(16)
        self.tcem: Signal = Signal(range(self.tcem_periods + 1))

        self.look_valid: Signal = Signal(1)
        self.look_addr: Signal = Signal(16)
        self.look_data: Signal = Signal(8)

        m.d.comb += [
            self.mem_mapped.eq(1),
            self.mem_caps.eq(AccessKind.EXECUTE | AccessKind.WRITE | AccessKind.READ)
        ]

        m.d.sync += self.bus_claim.eq(1)

        self.elaborate_requests(m)
        self.elaborate_tcem(m)


    def elaborate_requests(self, m: Module) -> None:
        self.read_request: Signal = Signal(1)
        self.write_request: Signal = Signal(1)
        self.hit_look: Signal = Signal(1)
        self.hit_burst: Signal = Signal(1)
        self.tcem_ok: Signal = Signal(1)
        self.burst_resumable: Signal = Signal(1)
        self.burst_extendable: Signal = Signal(1)

        m.d.comb += [
            self.read_request.eq(self.mem_request & ~self.mem_write_enable),
            self.write_request.eq(self.mem_request & self.mem_write_enable),
            self.hit_look.eq(self.look_valid & (self.look_addr == self.mem_addr)),
            self.tcem_ok.eq(self.tcem < (self.tcem_periods - TCEM_MARGIN)),

            self.burst_resumable.eq(self.burst_open & self.tcem_ok),
            self.hit_burst.eq(self.burst_resumable & (self.burst_next == self.mem_addr)),

            self.burst_extendable.eq(
                self.tcem_ok & (self.burst_next[0:PAGE_BITS] != (1 << PAGE_BITS) - 1)
            )
        ]


    def elaborate_tcem(self, m: Module) -> None:
        with m.If(~self.cs_assert):
            m.d.sync += self.tcem.eq(0)
        with m.Elif(self.tick & self.tcem_ok):
            m.d.sync += self.tcem.eq(self.tcem + 1)


    def elaborate_pins(self, m: Module) -> None:
        for index in range(len(ChipSelect)):
            m.d.comb += self.cs_n[index].eq(~(self.cs_assert & (self.selected == index)))

        m.d.comb += self.sd_o.eq(
            Mux(self.quad, self.shift_out[28:32], Cat(self.shift_out[31], C(0, 1), C(0b11, 2)))
        )


    def count_period(self, m: Module) -> Value:
        done: Signal = Signal(1)
        with m.If(self.tick):
            m.d.sync += self.count.eq(self.count - 1)
            with m.If(self.count == 1):
                m.d.comb += done.eq(1)
        return done


    def shift_out_quad(self, m: Module) -> Value:
        m.d.comb += [self.clocking.eq(1), self.sd_oe.eq(SD_OE_QUAD)]
        with m.If(self.tick):
            m.d.sync += self.shift_out.eq(self.shift_out << 4)
        return self.count_period(m)


    def shift_in_quad(self, m: Module) -> Value:
        m.d.comb += [self.clocking.eq(1), self.sd_oe.eq(SD_OE_NONE)]
        with m.If(self.tick):
            m.d.sync += self.shift_in.eq(self.data_byte())
        return self.count_period(m)


    def shift_out_single(self, m: Module) -> Value:
        m.d.comb += [self.clocking.eq(1), self.sd_oe.eq(SD_OE_SINGLE)]
        with m.If(self.tick):
            m.d.sync += self.shift_out.eq(self.shift_out << 1)
        return self.count_period(m)


    def shift_in_single(self, m: Module) -> Value:
        m.d.comb += [self.clocking.eq(1), self.sd_oe.eq(SD_OE_SINGLE)]
        with m.If(self.tick):
            m.d.sync += self.shift_in.eq(self.identity_byte())
        return self.count_period(m)


    def data_byte(self) -> Value:
        return Cat(self.sample, self.shift_in[0:4])


    def identity_byte(self) -> Value:
        return Cat(self.sample[1], self.shift_in[0:7])


    def load_init_cmd(self, m: Module, opcode: QspiCmd, bits: int = 8) -> None:
        m.d.sync += [self.shift_out.eq(C(int(opcode), 8) << 24), self.count.eq(bits)]


    def start_frame(self, m: Module, opcode: QspiCmd) -> None:
        with m.If(self.burst_open):
            self.close_burst(m)
        with m.Else():
            m.d.sync += [
                self.shift_out.eq(C(int(opcode), 8) << 24),
                self.count.eq(CMD_PERIODS),
                self.cs_assert.eq(1),
                self.writing.eq(opcode == QspiCmd.WRITE)
            ]
            m.next = QspiState.CMD


    def close_burst(self, m: Module) -> None:
        m.d.sync += [self.cs_assert.eq(0), self.burst_open.eq(0)]
        m.next = QspiState.CLOSE


    def elaborate_power_up(self, m: Module) -> None:
        with m.If(self.startup >= self.power_up_cycles):
            m.next = QspiState.INIT_SETUP
        with m.Else():
            m.d.sync += self.startup.eq(self.startup + 1)


    def elaborate_init_setup(self, m: Module) -> None:
        with m.If(self.tick):
            m.d.sync += [self.cs_assert.eq(1), self.quad.eq(0)]
            with m.Switch(self.init_step):
                with m.Case(InitStep.RESET_EN):
                    self.load_init_cmd(m, QspiCmd.RESET_EN)
                with m.Case(InitStep.RESET):
                    self.load_init_cmd(m, QspiCmd.RESET)
                with m.Case(InitStep.PROBE):
                    self.load_init_cmd(m, QspiCmd.READ_ID, bits = PROBE_CMD_BITS)
                with m.Case(InitStep.QUAD):
                    self.load_init_cmd(m, QspiCmd.ENTER_QUAD)
            m.next = QspiState.INIT_SHIFT


    def elaborate_init_shift(self, m: Module) -> None:
        with m.If(self.shift_out_single(m)):
            with m.Switch(self.init_step):
                with m.Case(InitStep.PROBE):
                    m.d.sync += self.count.eq(IDENTITY_BITS)
                    m.next = QspiState.INIT_IDENTITY
                with m.Case(InitStep.QUAD):
                    m.d.sync += [self.cs_assert.eq(0), self.quad.eq(1)]
                    m.next = QspiState.CLOSE
                with m.Default():
                    m.d.sync += [self.cs_assert.eq(0), self.init_step.eq(self.init_step + 1)]
                    m.next = QspiState.INIT_GAP


    def elaborate_init_identity(self, m: Module) -> None:
        identity: Value = self.identity_byte()

        with m.If(self.shift_in_single(m)):
            m.d.sync += [self.cs_assert.eq(0), self.device_id.eq(identity)]
            with m.If(identity == self.psram_id):
                m.d.sync += [self.device_found.eq(1), self.init_step.eq(InitStep.QUAD)]
                m.next = QspiState.INIT_GAP
            with m.Elif(self.selected == ChipSelect.RAM_A):
                m.d.sync += [
                    self.selected.eq(ChipSelect.RAM_B),
                    self.init_step.eq(InitStep.RESET_EN)
                ]
                m.next = QspiState.INIT_GAP
            with m.Else():
                m.next = QspiState.DEAD_BUS


    def elaborate_init_gap(self, m: Module) -> None:
        with m.If(self.tick):
            m.next = QspiState.INIT_SETUP


    def elaborate_dead_bus(self, m: Module) -> None:
        m.d.comb += self.mem_ready.eq(self.mem_request)


    def elaborate_idle(self, m: Module) -> None:
        with m.If(self.read_request & self.hit_look):
            m.d.comb += [self.mem_read_data.eq(self.look_data), self.mem_ready.eq(1)]
            m.d.sync += self.look_valid.eq(0)
        with m.Elif(~self.tick):
            pass
        with m.Elif(self.write_request):
            m.d.sync += self.look_valid.eq(0)
            self.start_frame(m, QspiCmd.WRITE)
        with m.Elif(self.read_request & self.hit_burst):
            m.d.sync += self.count.eq(BYTE_PERIODS)
            m.next = QspiState.READ_DATA
        with m.Elif(self.read_request):
            m.d.sync += self.look_valid.eq(0)
            self.start_frame(m, QspiCmd.FAST_READ)
        with m.Elif(self.burst_open & ~self.tcem_ok):
            self.close_burst(m)
        with m.Elif(self.burst_resumable & ~self.look_valid):
            m.d.sync += self.count.eq(BYTE_PERIODS)
            m.next = QspiState.LOOKAHEAD


    def elaborate_close(self, m: Module) -> None:
        with m.If(self.tick):
            m.next = QspiState.IDLE


    def elaborate_cmd(self, m: Module) -> None:
        with m.If(self.shift_out_quad(m)):
            m.d.sync += [
                self.shift_out.eq(Cat(C(0, 8), self.mem_addr, C(0, 8))),
                self.count.eq(ADDR_PERIODS),
                self.burst_next.eq(self.mem_addr)
            ]
            m.next = QspiState.ADDR


    def elaborate_addr(self, m: Module) -> None:
        with m.If(self.shift_out_quad(m)):
            with m.If(self.writing):
                m.d.sync += [
                    self.shift_out.eq(Cat(C(0, 24), self.mem_write_data)),
                    self.count.eq(BYTE_PERIODS)
                ]
                m.next = QspiState.WRITE_DATA
            with m.Else():
                m.d.sync += self.count.eq(WAIT_PERIODS)
                m.next = QspiState.WAIT


    def elaborate_wait(self, m: Module) -> None:
        m.d.comb += [self.clocking.eq(1), self.sd_oe.eq(SD_OE_NONE)]
        with m.If(self.count_period(m)):
            m.d.sync += self.count.eq(BYTE_PERIODS)
            m.next = QspiState.READ_DATA


    def elaborate_read_data(self, m: Module) -> None:
        with m.If(self.shift_in_quad(m)):
            m.d.comb += [self.mem_read_data.eq(self.data_byte()), self.mem_ready.eq(1)]
            m.d.sync += self.burst_next.eq(self.burst_next + 1)
            with m.If(self.burst_extendable):
                m.d.sync += [self.burst_open.eq(1), self.count.eq(BYTE_PERIODS)]
                m.next = QspiState.LOOKAHEAD
            with m.Else():
                self.close_burst(m)


    def elaborate_lookahead(self, m: Module) -> None:
        served: Signal = Signal(1)

        with m.If(self.shift_in_quad(m)):
            m.d.comb += served.eq(self.read_request & (self.mem_addr == self.burst_next))
            with m.If(served):
                m.d.comb += [self.mem_read_data.eq(self.data_byte()), self.mem_ready.eq(1)]

            m.d.sync += [
                self.look_valid.eq(~served),
                self.look_addr.eq(self.burst_next),
                self.look_data.eq(self.data_byte()),
                self.burst_next.eq(self.burst_next + 1)
            ]
            with m.If(self.burst_extendable):
                m.next = QspiState.IDLE
            with m.Else():
                self.close_burst(m)


    def elaborate_write_data(self, m: Module) -> None:
        with m.If(self.shift_out_quad(m)):
            m.d.comb += self.mem_ready.eq(1)
            m.d.sync += self.writing.eq(0)
            self.close_burst(m)
