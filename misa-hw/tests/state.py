import base64
from dataclasses import dataclass, replace

from misa_hw.init import CoreInit, CsrRegInit, GpRegInit, MpuInit

from memory import MEMORY_SIZE, MemMap


GP_REG_NAMES: tuple = (
    "r0", "ra", "rb", "rc", "rd", "re", "rf", "ru",
    "rv", "rw", "rx", "ry", "rz", "rt", "rscratch0", "rscratch1"
)
CSR_REG_NAMES: tuple = (
    "saddr", "raddr", "flags", "cause", "extns", "fault", "retsc", "retir", "privs"
)
CSR_WRITE_MASKS: dict = {
    "saddr": 0xFFFF,
    "raddr": 0xFFFF,
    "flags": 0x000F,
    "cause": 0xFF3F,
    "extns": 0x000F,
    "fault": 0xFFFF,
    "retsc": 0xFFFF,
    "retir": 0xFFFF,
    "privs": 0x0001,
    "mpucf": 0xFFFF
}
SHADOW_MASKS: dict = {
    "int_rscratch": 0xFFFF,
    "int_flags": 0x000F,
    "syscall_privs": 0x0001,
    "int_privs": 0x0001
}
MPU_REGION_COUNT: int = 4
BOUNDARY_IGNORED: tuple = ("int_pending",)
MEMORY_DIFF_LIMIT: int = 20
_SECTION_NAMES: tuple = ("special", "registers", "caps", "mpu", "writes")


@dataclass
class SpecRecord:

    scalars: dict
    specials: dict
    registers: dict
    caps: list
    mpu: dict
    writes: list
    memory: str


def parse_spec_text(text: str) -> SpecRecord:
    record: SpecRecord = SpecRecord({}, {}, {}, [], {}, [], "")
    section: str = None

    for raw in text.split("\n"):
        line: str = raw.strip()
        if (not line) or (":" not in line):
            continue
        key, _, rest = line.partition(":")
        key = key.strip()
        value: str = rest.strip()

        if (key in _SECTION_NAMES) and (not value):
            section = key
        elif (key == "memory"):
            record.memory = value
        elif (section == "special"):
            record.specials[key] = value
        elif (section == "registers"):
            record.registers[key] = value
        elif (section == "caps"):
            record.caps.append((key, value))
        elif (section == "mpu"):
            record.mpu[key] = value
        elif (section == "writes"):
            record.writes.append((key, value))
        else:
            record.scalars[key] = value

    return record


def _word(text: str) -> int:
    return int(text, 0)


def _flag(text: str) -> bool:
    return text == "true"


@dataclass(frozen = True)
class MisaState:

    gp_regs: tuple
    csrs: dict
    mpu_regions: tuple
    mpu_index: int
    pc: int
    in_syscall: bool
    in_interrupt: bool
    int_pending: bool
    int_rscratch: int
    int_flags: int
    syscall_privs: int
    int_privs: int
    halted: bool
    memory: bytes


    @property
    def mpucf(self) -> int:
        return self.mpu_regions[self.mpu_index]


    def masked_csr(self, name: str) -> int:
        return self.csrs[name] & CSR_WRITE_MASKS[name]


    def to_spec_file(self, memmap: MemMap) -> str:
        scalars: list = [
            ("halted", "true" if (self.halted) else "false"),
            ("inSyscall", "true" if (self.in_syscall) else "false"),
            ("syscallPrivs", f"{self.syscall_privs:#06x}"),
            ("inInterrupt", "true" if (self.in_interrupt) else "false"),
            ("intPending", "true" if (self.int_pending) else "false"),
            ("intRscratch", f"{self.int_rscratch:#06x}"),
            ("intFlags", f"{self.int_flags:#06x}"),
            ("intPrivs", f"{self.int_privs:#06x}"),
            ("pc", f"{self.pc:#06x}")
        ]

        text: str = "".join(f"{name:<12}: {value}\n" for name, value in scalars)

        text += "special:\n"
        for name in CSR_REG_NAMES:
            text += f"  {name} : {self.csrs[name]:#06x}\n"
        text += f"  mpucf : {self.mpucf:#06x}\n"

        text += "registers:\n"
        for index, name in enumerate(GP_REG_NAMES):
            text += f"  {name} : {self.gp_regs[index]:#04x}\n"

        text += "caps:\n"
        text += memmap.to_state_file_section()

        text += "mpu:\n"
        for index in range(MPU_REGION_COUNT):
            text += f"  {index} : {self.mpu_regions[index]:#06x}\n"

        text += f"memory: {base64.b64encode(self.memory).decode('ascii')}\n"
        return text


    @classmethod
    def from_spec_text(cls, text: str) -> tuple:
        record: SpecRecord = parse_spec_text(text)
        return (cls.from_record(record), MemMap.from_state_file_rows(tuple(record.caps)))


    @classmethod
    def from_record(cls, record: SpecRecord) -> "MisaState":
        scalars: dict = record.scalars
        mpucf: int = _word(record.specials.get("mpucf", "0"))
        index: int = (mpucf >> 3) & 0b11

        regions: list = [
            _word(record.mpu.get(str(slot), "0")) for slot in range(MPU_REGION_COUNT)
        ]
        regions[index] = mpucf

        memory: bytes = base64.b64decode(record.memory) if (record.memory) else b""
        if (len(memory) < MEMORY_SIZE):
            memory = memory + bytes(MEMORY_SIZE - len(memory))

        return cls(
            gp_regs = tuple(
                0 if (slot == 0) else _word(record.registers.get(name, "0"))
                for slot, name in enumerate(GP_REG_NAMES)
            ),
            csrs = {
                name: _word(record.specials.get(name, "0")) & CSR_WRITE_MASKS[name]
                for name in CSR_REG_NAMES
            },
            mpu_regions = tuple(regions),
            mpu_index = index,
            pc = _word(scalars["pc"]),
            in_syscall = _flag(scalars.get("inSyscall", "false")),
            in_interrupt = _flag(scalars.get("inInterrupt", "false")),
            int_pending = _flag(scalars.get("intPending", "false")),
            int_rscratch = _word(scalars.get("intRscratch", "0")),
            int_flags = _word(scalars.get("intFlags", "0")),
            syscall_privs = _word(scalars.get("syscallPrivs", "0")),
            int_privs = _word(scalars.get("intPrivs", "0")),
            halted = _flag(scalars["halted"]),
            memory = bytes(memory)
        )


    def apply_delta(self, text: str) -> "MisaState":
        record: SpecRecord = parse_spec_text(text)
        scalars: dict = record.scalars

        csrs: dict = dict(self.csrs)
        for name, value in record.specials.items():
            if (name in csrs):
                csrs[name] = _word(value) & CSR_WRITE_MASKS[name]

        regions: list = list(self.mpu_regions)
        for slot, value in record.mpu.items():
            regions[int(slot)] = _word(value)
        index: int = self.mpu_index
        if ("mpucf" in record.specials):
            mpucf: int = _word(record.specials["mpucf"])
            index = (mpucf >> 3) & 0b11
            regions[index] = mpucf

        gp_regs: list = list(self.gp_regs)
        for name, value in record.registers.items():
            gp_regs[GP_REG_NAMES.index(name)] = _word(value)

        memory: bytes = self.memory
        if (record.writes):
            buffer: bytearray = bytearray(self.memory)
            for addr, value in record.writes:
                buffer[_word(addr)] = _word(value)
            memory = bytes(buffer)

        return replace(
            self,
            gp_regs = tuple(gp_regs),
            csrs = csrs,
            mpu_regions = tuple(regions),
            mpu_index = index,
            pc = _word(scalars["pc"]) if ("pc" in scalars) else self.pc,
            in_syscall = _flag(scalars["inSyscall"]) if ("inSyscall" in scalars)
                         else self.in_syscall,
            in_interrupt = _flag(scalars["inInterrupt"]) if ("inInterrupt" in scalars)
                           else self.in_interrupt,
            int_pending = _flag(scalars["intPending"]) if ("intPending" in scalars)
                          else self.int_pending,
            int_rscratch = _word(scalars["intRscratch"]) if ("intRscratch" in scalars)
                           else self.int_rscratch,
            int_flags = _word(scalars["intFlags"]) if ("intFlags" in scalars) else self.int_flags,
            syscall_privs = _word(scalars["syscallPrivs"]) if ("syscallPrivs" in scalars)
                            else self.syscall_privs,
            int_privs = _word(scalars["intPrivs"]) if ("intPrivs" in scalars) else self.int_privs,
            halted = _flag(scalars["halted"]) if ("halted" in scalars) else self.halted,
            memory = memory
        )


    def diff(self, other: "MisaState", ignore: tuple = ()) -> list:
        diffs: list = []

        def compare(name: str, mine: object, theirs: object) -> None:
            if (name in ignore):
                return
            if (mine != theirs):
                diffs.append((name, mine, theirs))

        for index, name in enumerate(GP_REG_NAMES):
            compare(name, self.gp_regs[index], other.gp_regs[index])

        for name in CSR_REG_NAMES:
            compare(name, self.masked_csr(name), other.masked_csr(name))
        compare("mpucf", self.mpucf & CSR_WRITE_MASKS["mpucf"],
                other.mpucf & CSR_WRITE_MASKS["mpucf"])

        for slot in range(MPU_REGION_COUNT):
            compare(f"mpu[{slot}]", self.mpu_regions[slot], other.mpu_regions[slot])
        compare("mpu_index", self.mpu_index, other.mpu_index)

        compare("pc", self.pc, other.pc)
        compare("halted", self.halted, other.halted)
        compare("in_syscall", self.in_syscall, other.in_syscall)
        compare("in_interrupt", self.in_interrupt, other.in_interrupt)
        compare("int_pending", self.int_pending, other.int_pending)

        for name, mask in SHADOW_MASKS.items():
            compare(name, getattr(self, name) & mask, getattr(other, name) & mask)

        if (self.memory != other.memory) and ("memory" not in ignore):
            shown: int = 0
            for addr in range(MEMORY_SIZE):
                if (self.memory[addr] != other.memory[addr]):
                    diffs.append((f"memory[{addr:#06x}]",
                                  self.memory[addr], other.memory[addr]))
                    shown += 1
                    if (shown >= MEMORY_DIFF_LIMIT):
                        diffs.append(("memory", "...", "further differences not shown"))
                        break

        return diffs


    def to_core_init(self) -> CoreInit:
        return CoreInit(
            register = GpRegInit(*self.gp_regs[1:]),
            special = CsrRegInit(**{name: self.masked_csr(name) for name in CSR_REG_NAMES}),
            mpu = MpuInit(regions = self.mpu_regions, index = self.mpu_index),
            pc = self.pc,
            in_syscall = self.in_syscall,
            in_interrupt = self.in_interrupt,
            int_pending = self.int_pending,
            int_rscratch = self.int_rscratch,
            int_flags = self.int_flags & SHADOW_MASKS["int_flags"],
            syscall_privs = self.syscall_privs & SHADOW_MASKS["syscall_privs"],
            int_privs = self.int_privs & SHADOW_MASKS["int_privs"],
            halted = self.halted
        )


    def summary(self) -> str:
        return (f"pc={self.pc:#06x} halted={self.halted} "
                f"cause={self.masked_csr('cause'):#06x} fault={self.csrs['fault']:#06x} "
                f"flags={self.masked_csr('flags'):#03x} "
                f"sc={int(self.in_syscall)} ir={int(self.in_interrupt)}")
