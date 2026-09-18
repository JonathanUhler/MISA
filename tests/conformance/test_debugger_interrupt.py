"""
The debugger `interrupt` command raises the pin and enqueues an entry via the simulator hook. Drives
the callback directly (the module name has a hyphen, so it is loaded by path) to avoid the
interactive command loop.
"""

import importlib.util
import io
import sys
from pathlib import Path

_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_ROOT / "misa-sim"))
sys.path.insert(0, str(_ROOT / "cli"))

_spec = importlib.util.spec_from_file_location("misa_sim_cli", _ROOT / "misa-sim" / "misa-sim.py")
misa_sim = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(misa_sim)

from simulator import IRQ_BASE


def test_interrupt_command_enqueues_and_raises_pin():
    shell = misa_sim.Shell()
    misa_sim.Callbacks.callback_interrupt(shell, [0x05, 0x1234])
    assert shell.sim.machine.irq.pending() is True
    assert shell.sim.read_mem(IRQ_BASE + 0) == 1        # queue length
    assert shell.sim.read_mem(IRQ_BASE + 3) == 0x05     # head interrupt number
    assert shell.sim.read_mem(IRQ_BASE + 4) == 0x34     # argptr low byte
    assert shell.sim.read_mem(IRQ_BASE + 5) == 0x12     # argptr high byte


def test_info_registers_runs_after_irq_migration():
    shell = misa_sim.Shell()
    shell.stdout = io.StringIO()
    misa_sim.Callbacks.callback_info_registers(shell)
    output = shell.stdout.getvalue()
    assert "irq_pin" in output
    assert "irq_len" in output
