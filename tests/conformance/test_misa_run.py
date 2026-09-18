import importlib.util
import io
import sys
from pathlib import Path

_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_ROOT / "misa-harness"))
sys.path.insert(0, str(_ROOT / "misa-sim"))

from clock import VirtualClock
from uart import Uart, UartStruct
from uart_console import UartConsole
from constants import UART_BASE

_spec = importlib.util.spec_from_file_location("misa_run", _ROOT / "misa-sim" / "misa-run.py")
misa_run = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(misa_run)


class EchoFakeSim:
    """Stand-in CPU: each step echoes one waiting UART RX byte to TX, halts after a newline."""

    def __init__(self, uart):
        self._uart = uart
        self.in_reset = False

    def step(self):
        if (self._uart.read(UartStruct.RX_VALID)):
            b = self._uart.read(UartStruct.RX_DATA)
            self._uart.write(UartStruct.TX_DATA, b)
            if (b == 0x0A):
                self.in_reset = True


class SpinFakeSim:

    def __init__(self):
        self.in_reset = False


    def step(self):
        pass


def _uart():
    u = Uart(UART_BASE, baud=1_000_000, frame_bits=10)
    u.attach(VirtualClock(), None)
    return u


def test_run_echoes_piped_input_until_halt():
    uart = _uart()
    console = UartConsole(uart, echo=False)
    inputs = iter([b"hi\n"])
    out = io.BytesIO()
    misa_run.run(EchoFakeSim(uart), console, lambda: next(inputs, b""), out,
                 steps_per_pump=1000, max_steps=100)
    assert out.getvalue() == b"hi\n"


def test_run_stops_at_max_steps():
    uart = _uart()
    console = UartConsole(uart, echo=False)
    sim = SpinFakeSim()
    steps = misa_run.run(sim, console, lambda: b"", io.BytesIO(),
                         steps_per_pump=10, max_steps=25)
    assert steps == 25
    assert sim.in_reset is False
