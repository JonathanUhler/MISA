"""Co-simulation harness: step the Amaranth core and misa-spec from the same state and compare.

`misa-spec` is the functional oracle. `step_reference` runs the oracle one instruction via its
arbitrary-state `qi` mode; `run_hw` runs the RTL the same way. Both consume and produce the same
state dict so a comparison is a full equivalence check, not a spot check.

State dict shape:
    halted       : bool
    inSyscall    : bool
    inInterrupt  : bool
    syscallPrivs : int (0/1 meaningful)
    intRscratch  : int (16-bit)
    intFlags     : int (4-bit meaningful)
    intPrivs     : int (0/1 meaningful)
    pc           : int (16-bit)
    special      : dict name -> int  (saddr raddr flags cause extns [retsc retir privs])
    registers    : dict name -> int  (r0..rt, rscratch0, rscratch1)
    memory       : bytes/bytearray of 65536

misa-spec does not take interrupts while stepping (`execute` is pure instruction semantics), so the
harness holds irq low; interrupt entry is covered by directed tests.
"""

import base64
import os
import shutil
import subprocess
import tempfile

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.core import Core
from misa_hw.config import CoreInit, SpecInit
from misa_hw.types import (
    EXT_SYSCALL, EXT_PRIVILEGE, EXT_INTERRUPT,
)


REG_NAMES: list = ["r0", "ra", "rb", "rc", "rd", "re", "rf", "ru",
                   "rv", "rw", "rx", "ry", "rz", "rt", "rscratch0", "rscratch1"]
# CSR name -> (SpecFile attribute, always-available?)
CSR_ORDER: list = ["saddr", "raddr", "flags", "cause", "extns", "retsc", "retir", "privs"]
CSR_EXT_BIT: dict = {"retsc": EXT_SYSCALL, "retir": EXT_INTERRUPT, "privs": EXT_PRIVILEGE}


def spec_exe() -> str:
    """Locate the misa-spec-exe oracle, or return None if unavailable."""
    found: str = shutil.which("misa-spec-exe")
    if found:
        return found
    repo: str = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
    cand: str = os.path.join(repo, "install", "misa-spec-exe")
    return cand if os.path.exists(cand) else None


def _extns_on(state: dict, bit: int) -> bool:
    return bool(state["special"]["extns"] & (1 << bit))


def serialize_state(state: dict) -> str:
    """Render a state dict as a misa-spec `qi` state file (mirrors State.serialize)."""
    lines: list = [f"halted      : {'true' if state['halted'] else 'false'}"]
    if _extns_on(state, EXT_SYSCALL):
        lines.append(f"inSyscall   : {'true' if state['inSyscall'] else 'false'}")
        lines.append(f"syscallPrivs: 0x{state['syscallPrivs'] & 0xFFFF:04x}")
    if _extns_on(state, EXT_INTERRUPT):
        lines.append(f"inInterrupt : {'true' if state['inInterrupt'] else 'false'}")
        lines.append(f"intRscratch : 0x{state['intRscratch'] & 0xFFFF:04x}")
        lines.append(f"intFlags    : 0x{state['intFlags'] & 0xFFFF:04x}")
        lines.append(f"intPrivs    : 0x{state['intPrivs'] & 0xFFFF:04x}")
    lines.append(f"pc          : 0x{state['pc'] & 0xFFFF:04x}")
    lines.append("special:")
    for name in CSR_ORDER:
        bit: int = CSR_EXT_BIT.get(name)
        if bit is None or _extns_on(state, bit):
            lines.append(f"  {name} : 0x{state['special'].get(name, 0) & 0xFFFF:04x}")
    lines.append("registers:")
    for name in REG_NAMES:
        lines.append(f"  {name} : 0x{state['registers'].get(name, 0) & 0xFF:02x}")
    mem: bytes = bytes(state["memory"])
    assert len(mem) == 65536
    lines.append("memory: " + base64.b64encode(mem).decode("ascii"))
    return "\n".join(lines) + "\n"


def parse_state(text: str) -> dict:
    """Parse misa-spec serialized output into a state dict."""
    scalars, special, registers, mem_b64 = {}, {}, {}, ""
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
    return {
        "halted": b("halted"),
        "inSyscall": b("inSyscall"),
        "inInterrupt": b("inInterrupt"),
        "syscallPrivs": w("syscallPrivs"),
        "intRscratch": w("intRscratch"),
        "intFlags": w("intFlags"),
        "intPrivs": w("intPrivs"),
        "pc": int(scalars["pc"], 0),
        "special": special,
        "registers": registers,
        "memory": base64.b64decode(mem_b64),
    }


def step_reference(state: dict, n: int = 1) -> dict:
    """Run the oracle for n instructions from `state` and return the resulting state dict."""
    exe: str = spec_exe()
    assert exe is not None, "misa-spec-exe not found"
    with tempfile.NamedTemporaryFile("w", suffix=".state", delete=False) as f:
        f.write(serialize_state(state))
        path: str = f.name
    try:
        res: subprocess.CompletedProcess = subprocess.run(
            [exe, "qi", path, str(n)], capture_output=True, text=True)
        assert res.returncode == 0, f"misa-spec-exe failed: {res.stderr}\n{res.stdout}"
        return parse_state(res.stdout)
    finally:
        os.unlink(path)


def _core_init(state: dict) -> CoreInit:
    regs: list = [state["registers"].get(name, 0) & 0xFF for name in REG_NAMES]
    special: SpecInit = SpecInit(**{name: state["special"].get(name, 0) & 0xFFFF
                                    for name in CSR_ORDER})
    return CoreInit(
        registers=regs,
        special=special,
        pc=state["pc"] & 0xFFFF,
        in_syscall=state["inSyscall"],
        in_interrupt=state["inInterrupt"],
        syscall_privs=state["syscallPrivs"],
        int_rscratch=state["intRscratch"],
        int_flags=state["intFlags"],
        int_privs=state["intPrivs"],
        halted=state["halted"],
    )


def run_hw(state: dict, n: int = 1, max_cycles: int = 4000) -> dict:
    """Run the RTL for n instructions from `state`, servicing memory in Python, and read it back."""
    if state["halted"]:
        # A halted machine does not change; the reference returns the input unchanged too.
        import copy
        return copy.deepcopy(state)

    extns: int = state["special"]["extns"]
    core: Core = Core(extns_reset=extns & 0xF, init=_core_init(state))
    mem: bytearray = bytearray(state["memory"])
    result: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(core.irq, 0)
        retires: int = 0
        prev_fetch: int = 1  # start cycle is already in FETCH0 (arbitrary-state entry)
        pending: tuple = None  # (addr, we, wdata) captured when mem_req is seen
        for _ in range(max_cycles):
            # Service the single-byte memory handshake (one-cycle latency, like IdealMemory).
            if pending is not None:
                addr, we, wdata = pending
                if we:
                    mem[addr] = wdata & 0xFF
                ctx.set(core.mem_rdata, mem[addr])
                ctx.set(core.mem_ready, 1)
                pending = None
            else:
                ctx.set(core.mem_ready, 0)
                if ctx.get(core.mem_req):
                    pending = (ctx.get(core.mem_addr), ctx.get(core.mem_we),
                               ctx.get(core.mem_wdata))
            if ctx.get(core.halted):
                break
            fetch: int = ctx.get(core.dbg_fetch)
            if fetch and not prev_fetch:
                retires += 1
                if retires >= n:
                    break
            prev_fetch = fetch
            await ctx.tick()
        result["regs"] = [0] + [ctx.get(core._regfile.regs[i]) for i in range(1, 16)]
        result["special"] = {
            "saddr": ctx.get(core._spec.saddr), "raddr": ctx.get(core._spec.raddr_r),
            "flags": ctx.get(core._spec.flags), "cause": ctx.get(core._spec.cause),
            "extns": ctx.get(core._spec.extns), "retsc": ctx.get(core._spec.retsc),
            "retir": ctx.get(core._spec.retir), "privs": ctx.get(core._spec.privs),
        }
        result["pc"] = ctx.get(core._pc)
        result["halted"] = bool(ctx.get(core.halted))
        result["inSyscall"] = bool(ctx.get(core._in_syscall))
        result["inInterrupt"] = bool(ctx.get(core._in_interrupt))
        result["syscallPrivs"] = ctx.get(core._syscall_privs)
        result["intRscratch"] = ctx.get(core._int_rscratch)
        result["intFlags"] = ctx.get(core._int_flags)
        result["intPrivs"] = ctx.get(core._int_privs)

    sim: Simulator = Simulator(core)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()

    return {
        "halted": result["halted"],
        "inSyscall": result["inSyscall"],
        "inInterrupt": result["inInterrupt"],
        "syscallPrivs": result["syscallPrivs"],
        "intRscratch": result["intRscratch"],
        "intFlags": result["intFlags"],
        "intPrivs": result["intPrivs"],
        "pc": result["pc"],
        "special": {name: result["special"][name] for name in CSR_ORDER},
        "registers": {name: result["regs"][i] for i, name in enumerate(REG_NAMES)},
        "memory": bytes(mem),
    }


def diff_states(ref: dict, hw: dict) -> list:
    """Return a list of human-readable differences between a reference and HW state.

    Compares the architecturally-defined bits only: EXTNS low nibble and PRIVS bit 0 (misa-spec
    keeps undefined high bits of a WSR that the HW does not store). Only CSRs the reference exposes
    (per its extension set) are compared.
    """
    diffs: list = []
    if ref["halted"] != hw["halted"]:
        diffs.append(f"halted: ref={ref['halted']} hw={hw['halted']}")
    if (ref["pc"] & 0xFFFF) != (hw["pc"] & 0xFFFF):
        diffs.append(f"pc: ref={ref['pc']:#06x} hw={hw['pc']:#06x}")
    for flag in ("inSyscall", "inInterrupt"):
        if ref[flag] != hw[flag]:
            diffs.append(f"{flag}: ref={ref[flag]} hw={hw[flag]}")
    for name in REG_NAMES:
        r, h = ref["registers"].get(name, 0) & 0xFF, hw["registers"][name] & 0xFF
        if r != h:
            diffs.append(f"reg {name}: ref={r:#04x} hw={h:#04x}")
    for name, rval in ref["special"].items():
        hval: int = hw["special"].get(name, 0)
        mask: int = 0xF if name == "extns" else (0x1 if name == "privs" else 0xFFFF)
        if (rval & mask) != (hval & mask):
            diffs.append(f"csr {name}: ref={rval:#06x} hw={hval:#06x} (mask {mask:#x})")
    if bytes(ref["memory"]) != bytes(hw["memory"]):
        for i in range(65536):
            if ref["memory"][i] != hw["memory"][i]:
                diffs.append(f"mem[{i:#06x}]: ref={ref['memory'][i]:#04x} hw={hw['memory'][i]:#04x}")
    return diffs
