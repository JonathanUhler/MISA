import importlib.util
import io
import sys
import tempfile
from pathlib import Path

_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_ROOT / "misa-harness"))
sys.path.insert(0, str(_ROOT / "misa-sim"))

from harness import assemble
from simulator import Simulator
from uart import Uart
from uart_console import UartConsole
from constants import UART_BASE

_spec = importlib.util.spec_from_file_location("misa_run", _ROOT / "misa-sim" / "misa-run.py")
misa_run = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(misa_run)

_ECHO_SRC = (_ROOT / "demos" / "echo" / "echo.S").read_text()


def test_echo_demo_echoes_lines_and_quits_on_blank_line():
    with tempfile.TemporaryDirectory() as workdir:
        bin_path = assemble(_ECHO_SRC, workdir)
        sim = Simulator()
        uart = Uart(UART_BASE)
        sim.machine.add(uart)
        sim.load_mem(str(bin_path))

        console = UartConsole(uart, echo=False)
        inputs = iter([b"hi\nyo\n\n"])       # two lines then a blank line to quit
        out = io.BytesIO()
        misa_run.run(sim, console, lambda: next(inputs, b""), out, max_steps=500_000)

        assert out.getvalue() == b"hi\nyo\n"
        assert sim.in_reset is True
