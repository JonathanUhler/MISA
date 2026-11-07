import random
from dataclasses import fields

from misa_hw.grammar import Extn, Op, Reg
from misa_hw.init import RegInit, CsrInit

from harness import CosimState


def uniform_state(rng: random.Random) -> CosimState:
    reg_init: RegInit = RegInit(**{f.name: rng.randint(0, 0xFF) for f in fields(RegInit)})

    extns: int = rng.randint(0, 0xF)
    csr_init_values: dict = dict(
        saddr = rng.randint(0, 0xFFFF), raddr = rng.randint(0, 0xFFFF), flags = rng.randint(0, 0xF),
        cause = rng.randint(0, 0xFFFF), extns = extns
    )
    if (extns & Extn.SYSCALL):
        csr_init_values["retsc"] = rng.randint(0, 0xFFFF)
    if (extns & Extn.INTERRUPT):
        csr_init_values["retir"] = rng.randint(0, 0xFFFF)
    if (extns & Extn.PRIVILEGE):
        csr_init_values["privs"] = rng.randint(0, 1)
    csr_init: CsrInit = CsrInit(**csr_init_values)

    pc: int = rng.randint(0, 0xFF00) & 0xFFFE

    in_syscall: bool = False
    in_interrupt: bool = False
    syscall_privs: int = 0
    int_rscratch: int = 0
    int_flags: int = 0
    int_privs: int = 0
    if ((extns & Extn.SYSCALL) and rng.random() < 0.3):
        in_syscall = True
        syscall_privs = rng.randint(0, 1)
    elif ((extns & Extn.INTERRUPT) and rng.random() < 0.3):
        in_interrupt = True
        int_rscratch = rng.randint(0, 0xFFFF)
        int_flags = rng.randint(0, 0xF)
        int_privs = rng.randint(0, 1)

    mem: bytearray = bytearray(rng.randint(0, 255) for _ in range(65536))
    word: int = rng.randint(0, 0xFFFF)
    mem[pc] = word & 0xFF
    mem[pc + 1] = (word >> 8) & 0xFF

    return CosimState(
        registers = reg_init,
        special = csr_init,
        pc = pc,
        in_syscall = in_syscall,
        in_interrupt = in_interrupt,
        syscall_privs = syscall_privs,
        int_rscratch = int_rscratch,
        int_flags = int_flags,
        int_privs = int_privs,
        halted = False,
        memory = bytes(mem)
    )


def biased_state(rng: random.Random) -> CosimState:
    base_pc: int = 0xC000

    reg_init: RegInit = RegInit(**{f.name: rng.randint(0, 0xFF) for f in fields(RegInit)})

    mem: bytearray = bytearray(65536)

    for i in range(80):
        op: int = rng.randint(Op.ADD, Op.SET)
        rd: int = rng.randint(Reg.R0, Reg.RSCRATCH1)
        if (op == Op.SET):
            word: int = Op.SET | (rd << 4) | (rng.randint(0, 255) << 8)
        elif (op == Op.RRC):
            word = Op.RRC | (rd << 4) | (rng.randint(0, 15) << 8)
        else:
            word = op | (rd << 4) | (rng.randint(0, 15) << 8) | (rng.randint(0, 15) << 12)

        addr: int = base_pc + 2 * i
        mem[addr] = word & 0xFF
        mem[addr + 1] = (word >> 8) & 0xFF

    return CosimState(
        registers = reg_init,
        special = CsrInit(extns = 0x00),
        pc = base_pc,
        memory = bytes(mem)
    )


def _body_line(rng: random.Random) -> str:
    # Straight-line compute on ra/rd/re only (rb/rc hold saved RADDR across a nested call).
    choice: int = rng.randint(0, 2)
    if (choice == 0):
        return f"        set ra 0x{rng.randint(0, 255):02x}"
    if (choice == 1):
        return "        add rd ra re"
    return "        xor re rd ra"


def callchain_source(rng: random.Random) -> str:
    depth: int = rng.randint(2, 5)
    lines: list = [
        "        set2 rscratch 0x01ff",
        "        wsr saddr rscratch",
        "        call _f0",
        "        halt rt",
    ]
    for i in range(depth):
        lines.append(f"_f{i}:")
        for _ in range(rng.randint(1, 3)):
            lines.append(_body_line(rng))
        if i < depth - 1:
            lines.append("        rsr raddr rb rc")   # save this frame's return address
            lines.append("        push rb")
            lines.append("        push rc")
            lines.append(f"        call _f{i + 1}")
            lines.append("        pop rc")
            lines.append("        pop rb")
            lines.append("        wsr raddr rb rc")   # restore before returning
        lines.append("        ret")
    return "\n".join(lines) + "\n"
