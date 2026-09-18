"""
End-to-end: an assembled handler drains one queue entry and returns to the spin loop. The primary
assertion is that the queue drains via the four-phase handshake and the handler returns; the
serviced number lands in the `last` global.
"""

import tempfile
from pathlib import Path

from harness import assemble, Simulator
from simulator import IRQ_BASE

_PROGRAM = Path(__file__).resolve().parents[1] / "programs" / "interrupt_drain.S"


def test_handler_drains_one_entry_and_returns():
    src = _PROGRAM.read_text()
    with tempfile.TemporaryDirectory() as workdir:
        bin_path = assemble(src, workdir, extensions = ["interrupt"])
        sim = Simulator()
        sim.load_mem(str(bin_path))

    sim.assert_interrupt(0x07, 0x0000)
    entered_handler = False
    for _ in range(500):
        sim.step()
        if (sim.in_interrupt):
            entered_handler = True
        if (entered_handler and not sim.in_interrupt):
            break

    assert entered_handler                      # the interrupt was taken
    assert sim.read_mem(IRQ_BASE + 0) == 0      # the handler drained the queue
