import os
import shutil
import subprocess
import tempfile

from amaranth import Module
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.core import Core
from misa_hw.init import CoreInit, CsrInit
from misa_hw.memory import IdealMemory


_EXT_BIT: dict = {"dynamichw": 0, "syscall": 1, "privilege": 2, "interrupt": 3}


def put16(img: list, addr: int, word: int) -> None:
    img[addr] = word & 0xFF
    img[addr + 1] = (word >> 8) & 0xFF


def extns_bits(names: tuple) -> int:
    bits: int = 0
    for name in names:
        bits |= 1 << _EXT_BIT[name]
    return bits


def _install_dir() -> str:
    return os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "install"))


def spec_exe() -> str:
    found: str = shutil.which("misa-spec-exe")
    if (found is not None):
        return found
    cand: str = os.path.join(_install_dir(), "misa-spec-exe")
    return cand if os.path.exists(cand) else None


def toolchain_dir() -> str:
    found: str = shutil.which("misa-as")
    if (found is not None):
        return os.path.dirname(found)
    cand: str = _install_dir()
    return cand if os.path.exists(os.path.join(cand, "misa-as")) else None


def assemble(text: str, extns: tuple = (), reset: str = "_start", interrupt: str = None,
             syscall: str = None, fault: str = None, as_extns: tuple = None) -> list:
    tc: str = toolchain_dir()
    assert tc is not None, "misa-as toolchain not found"
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
        with open(src, "w", encoding = "utf-8") as f:
            f.write(source)
        env: dict = {**os.environ, "PATH": tc + os.pathsep + os.environ.get("PATH", "")}
        args: list = [os.path.join(tc, "misa-as"), src, "-o", out]
        if flags:
            args += ["-e", *flags]
        res: subprocess.CompletedProcess = subprocess.run(
            args, capture_output = True, text = True, env = env)
        assert res.returncode == 0, f"misa-as failed:\n{source}\n{res.stdout}\n{res.stderr}"
        with open(out, "rb") as f:
            return list(f.read())


def _ideal_memory_core(image: list, extns: int) -> tuple:
    if (extns is None):
        core: Core = Core()
    else:
        pc0: int = image[0xFFFE] | (image[0xFFFF] << 8)
        core = Core(init_data = CoreInit(pc = pc0, csr_init = CsrInit(extns = extns)))
    mem: IdealMemory = IdealMemory(init = image)
    return core, mem


def run_on_hw(image: list, extns: int = None, max_cycles: int = 5000) -> dict:
    m: Module = Module()
    core, mem = _ideal_memory_core(image, extns)
    m.submodules.core = core
    m.submodules.mem = mem
    m.d.comb += [
        mem.mem_addr.eq(core.mem_addr),
        mem.mem_write_data.eq(core.mem_write_data),
        mem.mem_write_enable.eq(core.mem_write_enable),
        mem.mem_request.eq(core.mem_request),
        core.mem_read_data.eq(mem.mem_read_data),
        core.mem_ready.eq(mem.mem_ready),
    ]
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(core.interrupt, 0)
        cycles: int = 0
        for i in range(max_cycles):
            cycles = i
            if ctx.get(core.halted):
                break
            await ctx.tick()
        out["halted"] = ctx.get(core.halted)
        out["halt_code"] = ctx.get(core.halt_code)
        out["cycles"] = cycles

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    return out
