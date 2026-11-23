from dataclasses import dataclass, field

from misa_hw.grammar import CsrReg, Extn, GpReg, Op

from memory import CAPS_EXECUTE, CAPS_RWX
from programs import (
    FAULT_VECTOR, INTERRUPT_VECTOR, PAGE_SIZE, PROGRAM_BASE, RESET_VECTOR, SYSCALL_VECTOR,
    alu_inst, block_image, csr_write_words, halt_inst, jal_inst, jmp_csr_inst, jmp_inst,
    load_inst, mpu_region, patterned_memory, rrc_inst, rsr_inst, set_inst, store_inst,
    syscall_inst, wsr_inst
)
from state import GP_REG_NAMES, MPU_REGION_COUNT, MisaState


HANDLER_BASE: int = 0xD000

HALT_CAUSE: int = 0x0001

GP_RESET: tuple = (0x00,) * (len(GP_REG_NAMES) - 1)

S1_REGISTERS: tuple = tuple(range(0x01, 0x10))
S3_REGISTERS: tuple = (0x00, 0x01, 0x00, 0x02, 0x00, 0x03, 0x00, 0x04,
                       0x00, 0x05, 0x00, 0x06, 0x00, 0x07, 0x00)
S4_REGISTERS: tuple = (0xFF, 0xFF, 0xFF, 0xF8, 0xFF, 0xF0, 0xFF, 0xFC,
                       0xFF, 0xFE, 0xFF, 0xFD, 0xFF, 0xFB, 0xFF)

STACK_MIDDLE: int = 0x8000
STACK_TOP: int = 0xFFFF

ALL_FLAGS: int = 0x000F
USER_MODE: int = 0x0001

PERMISSIVE_REGIONS: tuple = (
    mpu_region(0x00, 7, 0, CAPS_RWX), mpu_region(0x80, 7, 1, CAPS_RWX)
)
RESTRICTIVE_REGIONS: tuple = (
    mpu_region(PROGRAM_BASE >> 8, 0, 0, CAPS_EXECUTE),
)


@dataclass(frozen = True)
class Seed:

    name: str
    purpose: str
    registers: tuple = field(default = GP_RESET)
    saddr: int = field(default = 0x0000)
    flags: int = field(default = 0x0000)
    privs: int = field(default = 0x0000)
    mpu: tuple = field(default = ())
    syscall: bool = field(default = False)
    interrupt: bool = field(default = False)
    requires: int = field(default = 0x0)


    def prologue(self) -> tuple:
        words: list = []
        for region in self.mpu:
            words += csr_write_words(CsrReg.MPUCF, region)
        if (self.saddr != 0x0000):
            words += csr_write_words(CsrReg.SADDR, self.saddr)
        if (self.flags != 0x0000):
            words += csr_write_words(CsrReg.FLAGS, self.flags)
        if (self.privs != 0x0000):
            words += csr_write_words(CsrReg.PRIVS, self.privs)
        if (self.registers != GP_RESET):
            words += [set_inst(slot, self.registers[slot - 1])
                      for slot in range(1, len(GP_REG_NAMES))]
        if (self.syscall):
            words.append(syscall_inst(GpReg.R0))
        return tuple(words)


SEEDS: tuple = (
    Seed("S0", "the canonical reset state"),
    Seed("S1", "distinct register values and a stack in the middle of memory",
         registers = S1_REGISTERS, saddr = STACK_MIDDLE),
    Seed("S2", "S1 with every flag set",
         registers = S1_REGISTERS, saddr = STACK_MIDDLE, flags = ALL_FLAGS),
    Seed("S3", "register pairs and a stack at the bottom of memory",
         registers = S3_REGISTERS, saddr = 0x0000),
    Seed("S4", "register pairs and a stack at the top of memory",
         registers = S4_REGISTERS, saddr = STACK_TOP),
    Seed("S5", "User Mode with an MPU which grants everything",
         registers = S1_REGISTERS, saddr = STACK_MIDDLE, privs = USER_MODE,
         mpu = PERMISSIVE_REGIONS, requires = Extn.PRIVILEGE),
    Seed("S6", "User Mode with an MPU which grants instruction fetch and nothing else",
         registers = S1_REGISTERS, saddr = STACK_MIDDLE, privs = USER_MODE,
         mpu = RESTRICTIVE_REGIONS, requires = Extn.PRIVILEGE),
    Seed("S7", "inside a system call context",
         registers = S1_REGISTERS, saddr = STACK_MIDDLE, syscall = True,
         requires = Extn.SYSCALL),
    Seed("S8", "inside an interrupt context",
         registers = S1_REGISTERS, saddr = STACK_MIDDLE, interrupt = True,
         requires = Extn.INTERRUPT)
)


@dataclass(frozen = True)
class SeedProgram:

    seed: Seed
    image: bytes
    slot: int
    presteps: int
    interrupts: tuple


def seed_program(seed: Seed) -> SeedProgram:
    words: tuple = seed.prologue()
    slot: int = PROGRAM_BASE + 2 * len(words)
    if (slot + 1 >= PROGRAM_BASE + PAGE_SIZE):
        raise ValueError(
            f"the {seed.name} prologue is {len(words)} instructions, which pushes the "
            f"instruction under test to {slot:#06x}, outside the page at {PROGRAM_BASE:#06x}"
        )

    vectors: dict = {RESET_VECTOR: PROGRAM_BASE, FAULT_VECTOR: HANDLER_BASE}
    if (seed.syscall):
        vectors[SYSCALL_VECTOR] = slot
    if (seed.interrupt):
        vectors[INTERRUPT_VECTOR] = slot

    return SeedProgram(
        seed = seed,
        image = block_image({PROGRAM_BASE: words + (halt_inst(),)}, vectors,
                            patterned_memory()),
        slot = slot,
        presteps = len(words) + (1 if (seed.interrupt) else 0),
        interrupts = (len(words),) if (seed.interrupt) else ()
    )


def seed_problems(program: SeedProgram, state: MisaState) -> list:
    seed: Seed = program.seed
    problems: list = []

    def check(name: str, expected: object, produced: object) -> None:
        if (expected != produced):
            problems.append((name, expected, produced))

    check("pc", program.slot, state.pc)
    check("halted", False, state.halted)
    check("in_syscall", seed.syscall, state.in_syscall)
    check("in_interrupt", seed.interrupt, state.in_interrupt)
    check("int_pending", False, state.int_pending)

    for slot in range(1, len(GP_REG_NAMES)):
        check(GP_REG_NAMES[slot], seed.registers[slot - 1], state.gp_regs[slot])

    check("saddr", seed.saddr, state.csrs["saddr"])
    check("flags", seed.flags, state.masked_csr("flags"))
    check("privs", seed.privs, state.masked_csr("privs"))
    check("fault", 0x0000, state.csrs["fault"])
    check("cause", HALT_CAUSE if (seed.syscall) else 0x0000, state.masked_csr("cause"))

    for index in range(MPU_REGION_COUNT):
        expected: int = seed.mpu[index] if (index < len(seed.mpu)) else 0x0000
        check(f"mpu[{index}]", expected, state.mpu_regions[index])

    if (seed.syscall):
        check("retsc", program.slot, state.csrs["retsc"])
        check("syscall_privs", seed.privs, state.syscall_privs)
    if (seed.interrupt):
        check("retir", program.slot, state.csrs["retir"])
        check("int_rscratch", (seed.registers[-2] << 8) | seed.registers[-1],
              state.int_rscratch)
        check("int_flags", seed.flags, state.int_flags)
        check("int_privs", seed.privs, state.int_privs)

    return problems


ALL_ENCODINGS: range = range(1 << 16)

REPRESENTATIVE_REGISTERS: tuple = (GpReg.R0, GpReg.RA, GpReg.RSCRATCH0, GpReg.RSCRATCH1)
REPRESENTATIVE_BASES: tuple = (0x0, 0x1, 0xD, 0xE, 0xF)
REPRESENTATIVE_IMM4: tuple = (0x0, 0x1, 0x7, 0x8, 0xF)
REPRESENTATIVE_IMM8: tuple = (0x00, 0x01, 0x7F, 0x80, 0xFF)
REPRESENTATIVE_ALU_OPS: tuple = (Op.ADD, Op.ADC, Op.SUB, Op.SBB, Op.AND, Op.OR, Op.XOR)

CSR_ENCODINGS: range = range(1 << 4)
CMP_ENCODINGS: range = range(1 << 3)

HALT_RESERVED: tuple = (0x00, 0x01, 0x40)
NIBBLE_RESERVED: tuple = (0x0, 0x1, 0x8)
BIT_RESERVED: tuple = (0, 1)


def representative_encodings() -> tuple:
    words: set = set()

    for rs in REPRESENTATIVE_REGISTERS:
        for reserved in HALT_RESERVED:
            words.add(halt_inst(rs, reserved))
            words.add(syscall_inst(rs, reserved))

    for op in REPRESENTATIVE_ALU_OPS:
        for rd in REPRESENTATIVE_REGISTERS:
            for rs1 in REPRESENTATIVE_REGISTERS:
                for rs2 in REPRESENTATIVE_REGISTERS:
                    words.add(alu_inst(op, rd, rs1, rs2))

    for rd in REPRESENTATIVE_REGISTERS:
        for rs in REPRESENTATIVE_REGISTERS:
            for reserved in NIBBLE_RESERVED:
                words.add(rrc_inst(rd, rs, reserved))

    for rd in REPRESENTATIVE_REGISTERS:
        for imm in REPRESENTATIVE_IMM8:
            words.add(set_inst(rd, imm))

    for rd in REPRESENTATIVE_REGISTERS:
        for base in REPRESENTATIVE_BASES:
            for imm in REPRESENTATIVE_IMM4:
                words.add(load_inst(rd, base, imm))
                words.add(store_inst(rd, base, imm))

    for csr in CSR_ENCODINGS:
        for rs1 in REPRESENTATIVE_REGISTERS:
            for rs2 in REPRESENTATIVE_REGISTERS:
                words.add(rsr_inst(csr, rs1, rs2))
                words.add(wsr_inst(csr, rs1, rs2))

    for cmp_flag in CMP_ENCODINGS:
        for rs1 in REPRESENTATIVE_REGISTERS:
            for rs2 in REPRESENTATIVE_REGISTERS:
                for reserved in BIT_RESERVED:
                    words.add(jal_inst(cmp_flag, rs1, rs2, reserved))
                words.add(jmp_inst(cmp_flag, rs1, rs2))
        for csr in CSR_ENCODINGS:
            for reserved in NIBBLE_RESERVED:
                words.add(jmp_csr_inst(cmp_flag, csr, reserved))

    return tuple(sorted(words))
