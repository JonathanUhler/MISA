#!/usr/bin/env python3
"""Elaborate the TinyTapeout top-level to Verilog.

Usage: python scripts/elaborate.py [output.v]   (default: tt_um_misa.v)
"""

import sys

from amaranth.back import verilog

from misa_hw.top import TtUmMisa


def emit() -> str:
    top: TtUmMisa = TtUmMisa()
    ports: list = [
        top.clk, top.rst_n, top.ena,
        top.ui_in, top.uo_out,
        top.uio_in, top.uio_out, top.uio_oe,
    ]
    return verilog.convert(top, name="tt_um_misa", ports=ports)


def main() -> None:
    out: str = sys.argv[1] if len(sys.argv) > 1 else "tt_um_misa.v"
    text: str = emit()
    with open(out, "w") as f:
        f.write(text)
    print(f"wrote {out} ({len(text)} bytes)")


if __name__ == "__main__":
    main()
