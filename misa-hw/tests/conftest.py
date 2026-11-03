"""Shared helpers for driving the core against an idealized memory in simulation."""

import os
import shutil
import subprocess
import tempfile

from amaranth import Module
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.memory import IdealMemory


# Architecture-extension names (for misa-as -e) mapped to their EXTNS register bit.
_EXT_BIT: dict = {"dynamichw": 0, "syscall": 1, "privilege": 2, "interrupt": 3}


def make_image() -> list:
    return [0] * 65536


def put16(img: list, addr: int, word: int) -> None:
    img[addr] = word & 0xFF
    img[addr + 1] = (word >> 8) & 0xFF


def _connect(m: Module, core: object, mem: object) -> None:
    m.d.comb += [
        mem.mem_addr.eq(core.mem_addr),
        mem.mem_wdata.eq(core.mem_wdata),
        mem.mem_we.eq(core.mem_we),
        mem.mem_req.eq(core.mem_req),
        core.mem_rdata.eq(mem.mem_rdata),
        core.mem_ready.eq(mem.mem_ready),
    ]


def run_until_halt(image: list, extns_reset: int = 0, irq_schedule: dict = None,
                   max_cycles: int = 5000) -> dict:
    """Run a program image until the core halts. Returns a result dict.

    irq_schedule: optional dict {cycle_index: value} driving core.irq at given cycles.
    """
    from misa_hw.core import Core  # lazy: core.py appears in Task 8

    m: Module = Module()
    core: Core = Core(extns_reset=extns_reset)
    mem: IdealMemory = IdealMemory(init=image)
    m.submodules.core = core
    m.submodules.mem = mem
    _connect(m, core, mem)

    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        for i in range(max_cycles):
            if irq_schedule is not None and hasattr(core, "irq") and i in irq_schedule:
                ctx.set(core.irq, irq_schedule[i])
            if ctx.get(core.halted):
                break
            await ctx.tick()
        out["halted"] = ctx.get(core.halted)
        out["halt_code"] = ctx.get(core.halt_code)
        out["cycles"] = i

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    return out


def extns_bits(names: tuple) -> int:
    """Convert a tuple of extension names to the matching EXTNS register value."""
    bits: int = 0
    for name in names:
        bits |= 1 << _EXT_BIT[name]
    return bits


def toolchain_dir() -> str:
    """Directory holding misa-as (from PATH, else the repo's install/); None if unavailable."""
    found: str = shutil.which("misa-as")
    if found is not None:
        return os.path.dirname(found)
    cand: str = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "install"))
    return cand if os.path.exists(os.path.join(cand, "misa-as")) else None


def assemble(text: str, extns: tuple = (), reset: str = "_start", interrupt: str = None,
             syscall: str = None, fault: str = None, as_extns: tuple = None) -> list:
    """Assemble a MISA text-section program plus a vector table into a 64 KiB image.

    `text` is the body of the `.section text` (the reset label is prepended automatically, so the
    program's first instruction is the reset entry). The vector table always fills all eight 2-byte
    slots: a handler slot gets `.addr <label>` when its label is given, otherwise zero.

    `as_extns` chooses the assembler's `-e` flags; it defaults to `extns`. Override it when a program
    must mention an extension's instructions/CSRs (so the assembler accepts them) while running with
    that extension disabled at reset — the case the Dynamic Hardware tests exercise.
    """
    tc: str = toolchain_dir()
    assert tc is not None, "misa-as toolchain not found on PATH or in install/"
    flags: tuple = extns if as_extns is None else as_extns

    def slot(label: str) -> str:
        return f"        .addr {label}" if label is not None else "        .array 0x00 0x00"

    source: str = (
        "        .section text\n"
        f"{reset}:\n"
        + text + "\n"
        "        .section vectors\n"
        "        .array 0x00 0x00\n"   # 0xFFF0 reserved
        "        .array 0x00 0x00\n"   # 0xFFF2 reserved
        "        .array 0x00 0x00\n"   # 0xFFF4 reserved
        "        .array 0x00 0x00\n"   # 0xFFF6 reserved
        + slot(interrupt) + "\n"       # 0xFFF8 interrupt handler
        + slot(syscall) + "\n"         # 0xFFFA syscall handler
        + slot(fault) + "\n"           # 0xFFFC fault handler
        + slot(reset) + "\n"           # 0xFFFE reset handler
    )

    with tempfile.TemporaryDirectory() as d:
        src: str = os.path.join(d, "prog.s")
        out: str = os.path.join(d, "prog.bin")
        with open(src, "w", encoding="utf-8") as f:
            f.write(source)
        env: dict = {**os.environ, "PATH": tc + os.pathsep + os.environ.get("PATH", "")}
        args: list = [os.path.join(tc, "misa-as"), src, "-o", out]
        if flags:
            args += ["-e", *flags]
        res: subprocess.CompletedProcess = subprocess.run(
            args, capture_output=True, text=True, env=env)
        assert res.returncode == 0, f"misa-as failed:\n{source}\n{res.stdout}\n{res.stderr}"
        with open(out, "rb") as f:
            return list(f.read())


def run_program(text: str, extns: tuple = (), reset: str = "_start", interrupt: str = None,
                syscall: str = None, fault: str = None, as_extns: tuple = None,
                irq_schedule: dict = None, max_cycles: int = 5000) -> dict:
    """Assemble a program and run it on the core until halt.

    `extns` drives the hardware's reset EXTNS (and, by default, the assembler). `as_extns` overrides
    only the assembler's `-e` flags (see assemble).
    """
    img: list = assemble(text, extns=extns, reset=reset, interrupt=interrupt, syscall=syscall,
                         fault=fault, as_extns=as_extns)
    return run_until_halt(img, extns_reset=extns_bits(extns), irq_schedule=irq_schedule,
                          max_cycles=max_cycles)
