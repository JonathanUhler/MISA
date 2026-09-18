#!/usr/bin/env python3


import os
import sys
from argparse import ArgumentParser, Namespace

_HARNESS = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "misa-harness")
if (_HARNESS not in sys.path):
    sys.path.insert(0, _HARNESS)

from simulator import Simulator, Csr
from uart import Uart
from constants import UART_BASE
from uart_console import UartConsole, open_raw_terminal, read_available


def run(sim, console, read_fn, out, steps_per_pump: int = 1000, max_steps=None) -> int:
    steps = 0
    while (not sim.in_reset):
        data = console.pump(read_fn())
        if (data):
            out.write(data)
            out.flush()
        for _ in range(steps_per_pump):
            if (sim.in_reset):
                break
            sim.step()
            steps += 1
            if (max_steps is not None and steps >= max_steps):
                return steps
    data = console.pump(b"")
    if (data):
        out.write(data)
        out.flush()
    return steps


def main() -> None:
    parser: ArgumentParser = ArgumentParser(
        description="Run a MISA binary with a UART console bridged to this terminal")
    parser.add_argument("binary", help="Flat binary memory image to load")
    parser.add_argument("--baud", type=int, default=9600, help="UART baud rate (virtual timing)")
    parser.add_argument("--load-offset", type=lambda s: int(s, 0), default=0x0000,
                        help="Address to load the binary at (default 0x0000)")
    parser.add_argument("--max-steps", type=int, default=None,
                        help="Optional instruction-count safety limit")
    args: Namespace = parser.parse_args()

    sim = Simulator()
    uart = Uart(UART_BASE, baud=args.baud)
    sim.machine.add(uart)
    sim.load_mem(args.binary, offset=args.load_offset)

    in_fd = sys.stdin.fileno()
    console = UartConsole(uart, echo=sys.stdin.isatty())
    out = sys.stdout.buffer

    sys.stderr.write(
        f"misa-run: loaded {args.binary}, UART at {UART_BASE:#06x}. Ctrl-C to quit.\n")
    sys.stderr.flush()

    steps = 0
    try:
        with open_raw_terminal(in_fd):
            steps = run(sim, console, lambda: read_available(in_fd), out,
                        max_steps=args.max_steps)
    except KeyboardInterrupt:
        sys.stderr.write("\nmisa-run: interrupted.\n")
        return

    halt_code = (sim.get_csr(Csr.CAUSE) >> 8) & 0xFF
    sys.stderr.write(f"\nmisa-run: halted (code {halt_code:#04x}) after {steps} steps.\n")


if (__name__ == "__main__"):
    main()
