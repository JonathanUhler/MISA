import base64
import os
import subprocess
import tempfile
from dataclasses import dataclass, field, fields

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.core import Core
from misa_hw.grammar import Extn
from misa_hw.init import CoreInit, RegInit, CsrInit

from testlib import spec_exe


# EXTNS masks that gate serialization of extension-specific fields.
_CSR_EXT_MASK: dict = {"retsc": Extn.SYSCALL, "retir": Extn.INTERRUPT, "privs": Extn.PRIVILEGE}


@dataclass
class CosimState:
    registers: RegInit = field(default_factory = RegInit)
    special: CsrInit = field(default_factory = CsrInit)
    pc: int = 0
    in_syscall: bool = False
    in_interrupt: bool = False
    syscall_privs: int = 0
    int_rscratch: int = 0
    int_flags: int = 0
    int_privs: int = 0
    halted: bool = False
    memory: bytes = field(default_factory = lambda: bytes(65536))


def _extns_on(state: CosimState, mask: int) -> bool:
    return bool(state.special.extns & mask)


def serialize_state(state: CosimState) -> str:
    lines: list = [f"halted      : {'true' if state.halted else 'false'}"]
    if (_extns_on(state, Extn.SYSCALL)):
        lines.append(f"inSyscall   : {'true' if state.in_syscall else 'false'}")
        lines.append(f"syscallPrivs: 0x{state.syscall_privs & 0xFFFF:04x}")
    if (_extns_on(state, Extn.INTERRUPT)):
        lines.append(f"inInterrupt : {'true' if state.in_interrupt else 'false'}")
        lines.append(f"intRscratch : 0x{state.int_rscratch & 0xFFFF:04x}")
        lines.append(f"intFlags    : 0x{state.int_flags & 0xFFFF:04x}")
        lines.append(f"intPrivs    : 0x{state.int_privs & 0xFFFF:04x}")
    lines.append(f"pc          : 0x{state.pc & 0xFFFF:04x}")

    lines.append("special:")
    for f in fields(CsrInit):
        mask: int = _CSR_EXT_MASK.get(f.name)
        if mask is None or _extns_on(state, mask):
            lines.append(f"  {f.name} : 0x{getattr(state.special, f.name) & 0xFFFF:04x}")

    lines.append("registers:")
    lines.append("  r0 : 0x00")
    for f in fields(RegInit):
        lines.append(f"  {f.name} : 0x{getattr(state.registers, f.name) & 0xFF:02x}")

    mem: bytes = bytes(state.memory)
    assert len(mem) == 65536, f"memory must be 65536 bytes, got {len(mem)}"
    lines.append("memory: " + base64.b64encode(mem).decode("ascii"))
    return "\n".join(lines) + "\n"


def parse_state(text: str) -> CosimState:
    scalars: dict = {}
    special: dict = {}
    registers: dict = {}
    mem_b64: str = ""
    section: int = 0
    for raw in text.splitlines():
        line: str = raw.strip()
        if not line:
            continue
        key, _, val = line.partition(":")
        key, val = key.strip(), val.strip()
        if key == "special" and val == "":
            section = 1
        elif key == "registers" and val == "":
            section = 2
        elif key == "memory":
            mem_b64 = val
        elif section == 1:
            special[key] = int(val, 0)
        elif section == 2:
            registers[key] = int(val, 0)
        else:
            scalars[key] = val

    def b(k: str) -> bool:
        return scalars.get(k, "false") == "true"

    def w(k: str) -> int:
        return int(scalars[k], 0) if k in scalars else 0

    reg_init: RegInit = RegInit(
        **{f.name: registers.get(f.name, 0) & 0xFF for f in fields(RegInit)}
    )
    csr_init: CsrInit = CsrInit(
        **{f.name: special.get(f.name, 0) & 0xFFFF for f in fields(CsrInit)}
    )
    return CosimState(
        registers = reg_init,
        special = csr_init,
        pc = int(scalars["pc"], 0),
        in_syscall = b("inSyscall"),
        in_interrupt = b("inInterrupt"),
        syscall_privs = w("syscallPrivs"),
        int_rscratch = w("intRscratch"),
        int_flags = w("intFlags"),
        int_privs = w("intPrivs"),
        halted = b("halted"),
        memory = base64.b64decode(mem_b64),
    )


def step_reference(state: CosimState, n: int = 1) -> CosimState:
    exe: str = spec_exe()
    assert exe is not None, "misa-spec-exe not found"
    with tempfile.NamedTemporaryFile("w", suffix = ".state", delete = False) as f:
        f.write(serialize_state(state))
        path: str = f.name

    try:
        res: subprocess.CompletedProcess = subprocess.run(
            [exe, "qi", path, str(n)], capture_output = True, text = True
        )
        assert res.returncode == 0, f"misa-spec-exe failed: {res.stderr}\n{res.stdout}"

        result: CosimState = parse_state(res.stdout)
        result.special.flags &= 0xF
        result.special.extns &= 0xF
        result.special.privs &= 0x1
        return result
    finally:
        os.unlink(path)


def core_init(state: CosimState) -> CoreInit:
    return CoreInit(
        reg_init = state.registers,
        csr_init = state.special,
        pc = state.pc,
        in_syscall = state.in_syscall,
        in_interrupt = state.in_interrupt,
        syscall_privs = state.syscall_privs,
        int_rscratch = state.int_rscratch,
        int_flags = state.int_flags,
        int_privs = state.int_privs,
        halted = state.halted
    )


def reset_state(image: list, extns: int) -> CosimState:
    pc: int = image[0xFFFE] | (image[0xFFFF] << 8)
    return CosimState(special = CsrInit(extns = extns), pc = pc, memory = bytes(image))


def _snapshot(ctx: SimulatorContext, core: Core, mem: bytearray) -> CosimState:
    regs: RegInit = RegInit(
        **{f.name: int(ctx.get(core.regfile.regs[i + 1])) for i, f in enumerate(fields(RegInit))}
    )
    special: CsrInit = CsrInit(
        **{f.name: int(ctx.get(getattr(core.csrfile, f.name))) for f in fields(CsrInit)}
    )
    return CosimState(
        registers = regs,
        special = special,
        pc = int(ctx.get(core.pc)),
        in_syscall = bool(ctx.get(core.in_syscall)),
        in_interrupt = bool(ctx.get(core.in_interrupt)),
        syscall_privs = int(ctx.get(core.syscall_privs)),
        int_rscratch = int(ctx.get(core.int_rscratch)),
        int_flags = int(ctx.get(core.int_flags)),
        int_privs = int(ctx.get(core.int_privs)),
        halted = bool(ctx.get(core.halted)),
        memory = bytes(mem)
    )


def run_hw_trace(state: CosimState, n: int, max_cycles: int = None) -> list:
    if (state.halted):
        return [state]
    if (max_cycles is None):
        max_cycles = 80 * n + 1000
    core: Core = Core(init_data = core_init(state))
    mem: bytearray = bytearray(state.memory)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(core.interrupt, 0)
        trace: list = []
        prev_fetch: int = 1
        pending: tuple = None
        for _ in range(max_cycles):
            if (pending is not None):
                addr, we, wdata = pending
                if we:
                    mem[addr] = wdata & 0xFF
                ctx.set(core.mem_read_data, mem[addr])
                ctx.set(core.mem_ready, 1)
                pending = None
            else:
                ctx.set(core.mem_ready, 0)
                if (ctx.get(core.mem_request)):
                    pending = (int(ctx.get(core.mem_addr)), int(ctx.get(core.mem_write_enable)),
                               int(ctx.get(core.mem_write_data)))
            if (ctx.get(core.halted)):
                trace.append(_snapshot(ctx, core, mem))
                break
            fetch: int = int(ctx.get(core.debug_fetch))
            if (fetch and not prev_fetch):
                trace.append(_snapshot(ctx, core, mem))
                if (len(trace) >= n):
                    break
            prev_fetch = fetch
            await ctx.tick()
        out["trace"] = trace

    sim: Simulator = Simulator(core)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    return out["trace"]


def diff_states(ref: CosimState, hw: CosimState) -> list:
    diffs: list = []
    extns: int = ref.special.extns
    if (ref.halted != hw.halted):
        diffs.append(f"halted: ref={ref.halted} hw={hw.halted}")
    if ((ref.pc & 0xFFFF) != (hw.pc & 0xFFFF)):
        diffs.append(f"pc: ref={ref.pc:#06x} hw={hw.pc:#06x}")
    if ((extns & Extn.SYSCALL) and ref.in_syscall != hw.in_syscall):
        diffs.append(f"in_syscall: ref={ref.in_syscall} hw={hw.in_syscall}")
    if ((extns & Extn.INTERRUPT) and ref.in_interrupt != hw.in_interrupt):
        diffs.append(f"in_interrupt: ref={ref.in_interrupt} hw={hw.in_interrupt}")

    for f in fields(RegInit):
        r: int = getattr(ref.registers, f.name) & 0xFF
        h: int = getattr(hw.registers, f.name) & 0xFF
        if (r != h):
            diffs.append(f"reg {f.name}: ref={r:#04x} hw={h:#04x}")

    for f in fields(CsrInit):
        ext_mask: int = _CSR_EXT_MASK.get(f.name)
        if (ext_mask is not None and not (extns & ext_mask)):
            continue
        mask: int = 0xF if f.name in ("extns", "flags") else (0x1 if f.name == "privs" else 0xFFFF)
        r = getattr(ref.special, f.name) & mask
        h = getattr(hw.special, f.name) & mask
        if (r != h):
            diffs.append(f"csr {f.name}: ref={r:#06x} hw={h:#06x} (mask {mask:#x})")

    if (bytes(ref.memory) != bytes(hw.memory)):
        for i in range(65536):
            if (ref.memory[i] != hw.memory[i]):
                diffs.append(f"mem[{i:#06x}]: ref={ref.memory[i]:#04x} hw={hw.memory[i]:#04x}")
                if (len(diffs) > 20):
                    break
    return diffs


def lockstep(initial: CosimState, n: int, from_reset: bool = False, max_cycles: int = None) -> None:
    hw_trace: list = run_hw_trace(initial, n, max_cycles = max_cycles)
    if (not (len(hw_trace) == n or (hw_trace and hw_trace[-1].halted))):
        raise AssertionError(
            f"core produced only {len(hw_trace)}/{n} retires without halting within the cycle "
            f"budget (stalled or over budget); refusing to report a vacuous pass"
        )

    cur: CosimState = initial
    for i, hw in enumerate(hw_trace):
        pre_pc: int = initial.pc if i == 0 else hw_trace[i - 1].pc
        cur = step_reference(cur, 1)
        d: list = diff_states(cur, hw)
        if (d):
            raise AssertionError(
                f"HW/oracle divergence at instruction {i + 1}/{len(hw_trace)} "
                f"(executed at pc={pre_pc:#06x}):\n  " + "\n  ".join(d)
            )
