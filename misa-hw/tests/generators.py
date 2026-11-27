from dataclasses import dataclass, field, replace
from random import Random

from misa_hw.grammar import CmpFlag, CsrReg, Extn, GpReg, Op

from memory import (
    CAPS_EXECUTE, CAPS_READ, CAPS_RWX, CAPS_WRITE, MEMORY_SIZE, MemMap, MemRegion
)
from programs import (
    FAULT_VECTOR, INTERRUPT_VECTOR, PAGE_SIZE, PROGRAM_BASE, RESET_VECTOR, SYSCALL_VECTOR,
    VECTOR_TABLE_LO, alu_inst, block_image, csr_write_words, halt_inst, jal_inst, jmp_csr_inst,
    jmp_inst, load_inst, mpu_region, patterned_memory, random_memory, rrc_inst, rsr_inst,
    set_inst, store_inst, syscall_inst, wsr_inst
)
from spec import SpecDriver
from state import (
    GP_REG_NAMES, MPU_REGION_COUNT, MisaState, USER_READABLE_CSRS, USER_WRITABLE_CSRS,
    csr_available
)


HANDLER_BASE: int = 0xD000
SYSCALL_BASE: int = 0xD100
INTERRUPT_BASE: int = 0xD200

DEFAULT_EXTNS: int = 0xF

CAMPAIGN_MEMMAP: MemMap = MemMap.everything()

DATA_PAIR: int = GpReg.RZ                                      # {RZ, RT}
ADDRESS_PAIR: int = GpReg.RSCRATCH0                            # {RSCRATCH0, RSCRATCH1}
STACK_PAIR: int = GpReg.RSCRATCH1                              # SADDR

CAMPAIGN_SEEDS: tuple = (24, 400)
PROFILE_SEEDS: tuple = (22, 90)

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

PROGRAM_PAGE: int = PROGRAM_BASE >> 8
USER_PAGE_REGION: int = mpu_region(PROGRAM_PAGE, 0, 0, CAPS_RWX)

USER_MODE_WORDS: tuple = (
    csr_write_words(CsrReg.MPUCF, USER_PAGE_REGION) + csr_write_words(CsrReg.PRIVS, USER_MODE)
)

ILLEGAL_WORD: int = rrc_inst(GpReg.RB, GpReg.RA, reserved = 0x1)


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


def seed_state(spec: SpecDriver, program: SeedProgram, extns: int) -> MisaState:
    state: MisaState = spec.q0(program.image, CAMPAIGN_MEMMAP, extns, steps = program.presteps,
                               interrupts = program.interrupts)[-1]
    problems: list = seed_problems(program, state)
    if (problems):
        raise AssertionError(
            f"the {program.seed.name} prologue did not produce the state it describes "
            f"({program.seed.purpose}):\n" + "\n".join(
                f"  {name}: the seed describes {expected!r}, the oracle produced {produced!r}"
                for name, expected, produced in problems
            )
        )
    return state


def seed_label(value: object) -> str:
    return value.name if (isinstance(value, Seed)) else f"extns{value:#x}"


def compatible_seeds(extns_values: tuple, seeds: tuple = SEEDS) -> tuple:
    return tuple(
        (seed, extns) for seed in seeds for extns in extns_values
        if ((extns & seed.requires) == seed.requires)
    )


def selected_seeds(exhaustive: bool, seed_count: int, seeds: tuple = SEEDS) -> tuple:
    if (seed_count is not None):
        return seeds[:seed_count]
    return seeds if (exhaustive) else seeds[:1]


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


PAIR_SEEDS: tuple = (SEEDS[1], SEEDS[0]) + SEEDS[2:]

PAIR_REGISTERS: tuple = (GpReg.R0, GpReg.RA, GpReg.RB, GpReg.RSCRATCH1)
PAIR_TRIPLES: tuple = (
    (GpReg.RA, GpReg.RA, GpReg.RB),
    (GpReg.RA, GpReg.RB, GpReg.RA),
    (GpReg.R0, GpReg.RA, GpReg.RB),
    (GpReg.RA, GpReg.RA, GpReg.RA),
    (GpReg.RSCRATCH1, GpReg.RA, GpReg.RB),
    (GpReg.RA, GpReg.RSCRATCH0, GpReg.RSCRATCH1)
)
PAIR_ROTATES: tuple = (
    (GpReg.RA, GpReg.RB), (GpReg.RA, GpReg.RA), (GpReg.R0, GpReg.RA),
    (GpReg.RSCRATCH0, GpReg.RA), (GpReg.RA, GpReg.RSCRATCH1), (GpReg.RSCRATCH1, GpReg.RSCRATCH0)
)
PAIR_ALU_OPS: tuple = (Op.ADD, Op.ADC, Op.SUB, Op.SBB, Op.AND, Op.OR, Op.XOR)
PAIR_CARRY_OPS: tuple = (Op.ADC, Op.SBB)
PAIR_ADDRESSES: tuple = (
    (GpReg.RA, GpReg.RB), (GpReg.R0, GpReg.RA), (GpReg.RSCRATCH0, GpReg.RSCRATCH1)
)
PAIR_BASES: tuple = (0x0, 0x1, 0xC, 0xE, 0xF)
PAIR_OFFSETS: tuple = (0x0, 0x1, 0x7, 0x8, 0xF)
PAIR_IMMEDIATES: tuple = (0x00, 0x01, 0x7F, 0x80, 0xFF)
PAIR_WRITTEN_CSRS: tuple = (CsrReg.FLAGS, CsrReg.SADDR, CsrReg.EXTNS, CsrReg.MPUCF)


def first_instructions() -> tuple:
    words: set = set()

    for op in PAIR_ALU_OPS:
        for rd, rs1, rs2 in PAIR_TRIPLES:
            words.add(alu_inst(op, rd, rs1, rs2))
    for rd, rs in PAIR_ROTATES:
        words.add(rrc_inst(rd, rs))

    for rd in PAIR_REGISTERS:
        for imm in PAIR_IMMEDIATES:
            words.add(set_inst(rd, imm))

    for base in PAIR_BASES:
        for imm in (0x0, 0x1, 0xF):
            words.add(load_inst(GpReg.RA, base, imm))
            words.add(store_inst(GpReg.RA, base, imm))

    for csr in CSR_ENCODINGS:
        words.add(wsr_inst(csr, GpReg.RA, GpReg.RB))
    for csr in PAIR_WRITTEN_CSRS:
        words.add(wsr_inst(csr, GpReg.R0, GpReg.R0))
        words.add(rsr_inst(csr, GpReg.RA, GpReg.RB))

    for cmp_flag in CMP_ENCODINGS:
        words.add(jal_inst(cmp_flag, GpReg.RA, GpReg.RB))
        words.add(jmp_inst(cmp_flag, GpReg.RA, GpReg.RB))

    words.add(halt_inst(GpReg.RA))
    words.add(syscall_inst(GpReg.RA))

    return tuple(sorted(words))


def second_instructions() -> tuple:
    words: set = set()

    for op in PAIR_CARRY_OPS:
        for rd, rs1, rs2 in PAIR_TRIPLES:
            words.add(alu_inst(op, rd, rs1, rs2))
    for op in (Op.ADD, Op.SUB, Op.AND, Op.OR, Op.XOR):
        for rd, rs1, rs2 in PAIR_TRIPLES[:3]:
            words.add(alu_inst(op, rd, rs1, rs2))
    for rd, rs in PAIR_ROTATES:
        words.add(rrc_inst(rd, rs))

    for cmp_flag in CMP_ENCODINGS:
        for rs1, rs2 in PAIR_ADDRESSES:
            words.add(jal_inst(cmp_flag, rs1, rs2))
            words.add(jmp_inst(cmp_flag, rs1, rs2))
        words.add(jmp_csr_inst(cmp_flag, CsrReg.RADDR))

    for csr in CSR_ENCODINGS:
        words.add(rsr_inst(csr, GpReg.RA, GpReg.RB))
        words.add(jmp_csr_inst(CmpFlag.ALWAYS, csr))
    for csr in PAIR_WRITTEN_CSRS:
        words.add(wsr_inst(csr, GpReg.RA, GpReg.RB))

    for base in PAIR_BASES:
        for imm in PAIR_OFFSETS:
            words.add(load_inst(GpReg.RA, base, imm))
            words.add(store_inst(GpReg.RA, base, imm))

    for imm in PAIR_IMMEDIATES:
        words.add(set_inst(GpReg.RA, imm))

    words.add(halt_inst(GpReg.RA))
    words.add(syscall_inst(GpReg.RA))

    return tuple(sorted(words))


def instruction_pairs(firsts: tuple, seconds: tuple) -> tuple:
    return tuple((first, second) for first in firsts for second in seconds)


CAMPAIGN_VECTORS: dict = {
    INTERRUPT_VECTOR: INTERRUPT_BASE, SYSCALL_VECTOR: SYSCALL_BASE,
    FAULT_VECTOR: HANDLER_BASE, RESET_VECTOR: PROGRAM_BASE
}


def directed_image(words: tuple, vectors: dict = None, blocks: dict = None,
                   data: dict = None, memory: bytearray = None) -> bytes:
    all_blocks: dict = {PROGRAM_BASE: words}
    all_blocks.update(blocks or {})

    all_vectors: dict = dict(CAMPAIGN_VECTORS)
    all_vectors.update(vectors or {})

    return block_image(all_blocks, all_vectors, memory, data)

PAGE_COUNT: int = MEMORY_SIZE // PAGE_SIZE
RANDOM_CAPS: tuple = (
    CAPS_RWX, CAPS_RWX, CAPS_RWX, CAPS_RWX, CAPS_RWX,
    CAPS_READ | CAPS_EXECUTE, CAPS_READ | CAPS_WRITE, CAPS_READ, CAPS_EXECUTE, 0b000, None
)

RANDOM_CURSOR: int = 0x00FF

RANDOM_GRANTED: tuple = (
    RANDOM_CURSOR, PROGRAM_BASE, HANDLER_BASE, SYSCALL_BASE, INTERRUPT_BASE, VECTOR_TABLE_LO
)

RANDOM_WALKER: tuple = (
    set_inst(GpReg.RSCRATCH0, (RANDOM_CURSOR >> 8) & 0xFF),
    set_inst(GpReg.RSCRATCH1, RANDOM_CURSOR & 0xFF),
    load_inst(GpReg.RT, ADDRESS_PAIR, 0x0),
    set_inst(GpReg.RZ, 0x01),
    alu_inst(Op.ADD, GpReg.RT, GpReg.RT, GpReg.RZ),
    store_inst(GpReg.RT, ADDRESS_PAIR, 0x0),
    set_inst(GpReg.RZ, 0x00),
    jmp_inst(CmpFlag.ALWAYS, GpReg.RT, GpReg.RZ)
)
RANDOM_RETURN: tuple = (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),)
RANDOM_BLOCKS: dict = {
    HANDLER_BASE: RANDOM_WALKER,
    SYSCALL_BASE: RANDOM_RETURN,
    INTERRUPT_BASE: RANDOM_WALKER
}


def random_image(seed: int) -> bytes:
    return block_image(RANDOM_BLOCKS, CAMPAIGN_VECTORS, random_memory(seed))


def coalesced_memmap(pages: list) -> MemMap:
    rows: list = []
    for page, caps in enumerate(pages):
        if (caps is None):
            continue
        if (rows) and (rows[-1][2] == caps) and (rows[-1][1] + 1 == page * PAGE_SIZE):
            rows[-1] = (rows[-1][0], (page + 1) * PAGE_SIZE - 1, caps)
        else:
            rows.append((page * PAGE_SIZE, (page + 1) * PAGE_SIZE - 1, caps))
    return MemMap(tuple(MemRegion(lo, hi, caps) for lo, hi, caps in rows))


def random_memmap(seed: int) -> MemMap:
    random: Random = Random(seed)
    pages: list = [random.choice(RANDOM_CAPS) for _ in range(PAGE_COUNT)]
    for addr in RANDOM_GRANTED:
        pages[addr >> 8] = CAPS_RWX
    return coalesced_memmap(pages)


STACK_RESET: int = 0x01FF
DATA_BASE: int = 0x0280
SUBROUTINE_BASE: int = 0xC800
SUBROUTINE_STRIDE: int = 0x40

PUSH_OFFSET: int = 0xF                                         # write, then SADDR - 1
POP_OFFSET: int = 0x1                                          # SADDR + 1, then read

RESERVED_REGISTERS: tuple = (GpReg.RZ, GpReg.RT)
BODY_REGISTERS: tuple = tuple(reg for reg in GpReg if (reg not in RESERVED_REGISTERS))
SEEDED_REGISTERS: tuple = (GpReg.RA, GpReg.RB, GpReg.RC, GpReg.RD, GpReg.RE, GpReg.RF)

USER_REGIONS: tuple = (
    mpu_region(PROGRAM_BASE >> 8, 4, 0, CAPS_READ | CAPS_EXECUTE),
    mpu_region(0x00, 1, 1, CAPS_READ | CAPS_WRITE),
    mpu_region(DATA_BASE >> 8, 0, 2, CAPS_RWX),
    mpu_region(0x80, 7, 3, 0b000)
)

HOLE_UNMAPPED: int = 0x8000
HOLE_DENIED: int = 0x0300
HOLE_READONLY: int = 0x0400

HOLED_ROWS: tuple = (
    (0x0000, 0x02FF, "rwx"), (HOLE_DENIED, 0x03FF, "-"), (HOLE_READONLY, 0x04FF, "r"),
    (0x0500, 0x7FFF, "rwx"), (0x9000, VECTOR_TABLE_LO - 1, "rwx"),
    (INTERRUPT_VECTOR, MEMORY_SIZE - 1, "rwx")
)

FAULT_HANDLER: tuple = (halt_inst(GpReg.R0),)
SYSCALL_HANDLER: tuple = (
    rsr_inst(CsrReg.CAUSE, GpReg.RSCRATCH0, GpReg.RSCRATCH1),
    jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC)
)
INTERRUPT_HANDLER: tuple = (halt_inst(GpReg.R0),)

SAFE_CSR_WRITES: tuple = (CsrReg.RADDR, CsrReg.FLAGS, CsrReg.CAUSE, CsrReg.FAULT)
MAX_STACK_DEPTH: int = 8
SUBROUTINE_LENGTH: int = 4
STEP_SLACK: int = 64

BASE_WEIGHTS: dict = {
    "alu": 26, "rotate": 4, "set": 12, "load": 9, "store": 9, "push": 5, "pop": 5,
    "branch": 6, "csr_read": 5, "csr_write": 5, "call": 4, "syscall": 4, "extns": 3, "mpu": 3
}
PLAIN_KINDS: tuple = ("alu", "rotate", "set", "load", "store", "csr_read")
FAULT_KINDS: tuple = ("illegal", "overflow", "vector", "unmapped", "denied", "readonly",
                      "privileged")


@dataclass(frozen = True)
class Knobs:

    name: str
    purpose: str
    length: int = 48
    fault_probability: float = 0.0
    extns: int = DEFAULT_EXTNS
    user_mode: bool = False
    holes: bool = False
    stack: bool = True
    call_depth: int = 0
    syscalls: bool = False
    dynamic: bool = False


    def __post_init__(self) -> None:
        if (self.dynamic) and (not (self.extns & Extn.DYNAMICHW)):
            raise ValueError(
                f"the {self.name} profile asks to write EXTNS with the Dynamic Hardware "
                f"Extension disabled, where the write is ignored and the knob does nothing"
            )
        if (self.dynamic) and (self.user_mode):
            raise ValueError(
                f"the {self.name} profile asks to write EXTNS from User Mode, where EXTNS is "
                f"not writable and every write would be a privileged instruction instead"
            )
        if (self.user_mode) and (not (self.extns & Extn.PRIVILEGE)):
            raise ValueError(
                f"the {self.name} profile asks for User Mode with the Privilege Extension "
                f"disabled, where WSR PRIVS is an illegal instruction and no program can enter it"
            )
        if (self.syscalls) and (not (self.extns & Extn.SYSCALL)):
            raise ValueError(
                f"the {self.name} profile asks for system calls with the System Call Extension "
                f"disabled, where SYSCALL is a HALT and the program would end at the first one"
            )
        if (self.call_depth) and (not self.stack) and (self.call_depth > 1):
            raise ValueError(
                f"the {self.name} profile nests calls {self.call_depth} deep with no stack, and "
                f"a subroutine which calls another has nowhere to save its return address"
            )


    def weights(self) -> dict:
        weights: dict = dict(BASE_WEIGHTS)
        if (not self.stack):
            weights["push"] = weights["pop"] = 0
        if (not self.call_depth):
            weights["call"] = 0
        if (not self.syscalls):
            weights["syscall"] = 0
        if (not self.dynamic):
            weights["extns"] = 0
        if (not (self.extns & Extn.PRIVILEGE)) or (self.user_mode):
            weights["mpu"] = 0
        if (not self.readable_csrs()):
            weights["csr_read"] = 0
        if (not self.writable_csrs()):
            weights["csr_write"] = 0
        return weights


    def readable_csrs(self) -> tuple:
        available: tuple = tuple(csr for csr in CsrReg if (csr_available(csr, self.extns)))
        if (self.user_mode):
            return tuple(csr for csr in available if (csr in USER_READABLE_CSRS))
        return available


    def writable_csrs(self) -> tuple:
        if (self.user_mode):
            return tuple(csr for csr in SAFE_CSR_WRITES if (csr in USER_WRITABLE_CSRS))
        return SAFE_CSR_WRITES


    def faults(self) -> tuple:
        kinds: list = ["illegal", "overflow", "vector"]
        if (self.holes):
            kinds += ["unmapped", "denied", "readonly", "unmapped_vector"]
        if (self.user_mode):
            kinds.append("privileged")
        return tuple(kinds)


    def memmap(self) -> MemMap:
        return MemMap.from_rows(HOLED_ROWS) if (self.holes) else CAMPAIGN_MEMMAP


@dataclass(frozen = True)
class Item:

    kind: str
    words: tuple
    target: int = None


def subroutine_address(index: int) -> int:
    return SUBROUTINE_BASE + SUBROUTINE_STRIDE * index


def call_words(addr: int) -> tuple:
    return (set_inst(GpReg.RSCRATCH0, (addr >> 8) & 0xFF),
            set_inst(GpReg.RSCRATCH1, addr & 0xFF),
            jal_inst(CmpFlag.ALWAYS, GpReg.RSCRATCH0, GpReg.RSCRATCH1))


def address_words(addr: int) -> tuple:
    return (set_inst(GpReg.RSCRATCH0, (addr >> 8) & 0xFF),
            set_inst(GpReg.RSCRATCH1, addr & 0xFF))


@dataclass(frozen = True)
class Program:

    knobs: Knobs
    seed: int
    prologue: tuple
    body: tuple
    epilogue: tuple
    subroutines: tuple
    memmap: MemMap
    steps: int


    def offsets(self) -> tuple:
        offsets: list = []
        offset: int = len(self.prologue)
        for item in self.body:
            offsets.append(offset)
            offset += len(item.words)
        return (tuple(offsets), offset)


    def main_words(self) -> tuple:
        offsets, end = self.offsets()
        words: list = list(self.prologue)
        for item in self.body:
            if (item.target is None):
                words += list(item.words)
                continue
            where: int = offsets[item.target] if (item.target < len(self.body)) else end
            words += list(address_words(PROGRAM_BASE + 2 * where)) + list(item.words[2:])
        return tuple(words) + self.epilogue


    def blocks(self) -> dict:
        blocks: dict = {
            PROGRAM_BASE: self.main_words(),
            HANDLER_BASE: FAULT_HANDLER,
            SYSCALL_BASE: SYSCALL_HANDLER,
            INTERRUPT_BASE: INTERRUPT_HANDLER
        }
        for index, words in enumerate(self.subroutines):
            blocks[subroutine_address(index)] = words
        return blocks


    def length(self) -> int:
        return sum(len(words) for words in self.blocks().values())


    def image(self) -> bytes:
        main: tuple = self.main_words()
        if (PROGRAM_BASE + 2 * len(main) > SUBROUTINE_BASE):
            raise ValueError(
                f"the {self.knobs.name} program is {len(main)} words, which runs from "
                f"{PROGRAM_BASE:#06x} into the subroutines at {SUBROUTINE_BASE:#06x}"
            )
        return block_image(self.blocks(), CAMPAIGN_VECTORS, patterned_memory())


    def kinds(self) -> tuple:
        return tuple(sorted({item.kind for item in self.body}))


class _Weighted:

    def __init__(self, knobs: Knobs, seed: int) -> None:
        self.knobs: Knobs = knobs
        self.random: Random = Random(f"{knobs.name}:{seed}")
        self.depth: int = 0


    def register(self) -> int:
        return self.random.choice(BODY_REGISTERS)


    def source(self) -> int:
        return self.random.choice(tuple(GpReg))


    def byte(self) -> int:
        return self.random.randrange(0x100)


    def nibble(self) -> int:
        return self.random.randrange(0x10)


    def item(self, kind: str, index: int = 0, length: int = 0) -> Item:
        if (kind == "alu"):
            return Item(kind, (alu_inst(self.random.choice(PAIR_ALU_OPS), self.register(),
                                        self.source(), self.source()),))
        if (kind == "rotate"):
            return Item(kind, (rrc_inst(self.register(), self.source()),))
        if (kind == "set"):
            return Item(kind, (set_inst(self.register(), self.byte()),))
        if (kind == "load"):
            return Item(kind, (load_inst(self.register(), DATA_PAIR, self.nibble()),))
        if (kind == "store"):
            return Item(kind, (store_inst(self.source(), DATA_PAIR, self.nibble()),))
        if (kind == "push"):
            self.depth += 1
            return Item(kind, (store_inst(self.source(), STACK_PAIR, PUSH_OFFSET),))
        if (kind == "pop"):
            self.depth -= 1
            return Item(kind, (load_inst(self.register(), STACK_PAIR, POP_OFFSET),))
        if (kind == "csr_read"):
            return Item(kind, (rsr_inst(self.random.choice(self.knobs.readable_csrs()),
                                        self.register(), self.register()),))
        if (kind == "csr_write"):
            return Item(kind, (wsr_inst(self.random.choice(self.knobs.writable_csrs()),
                                        self.source(), self.source()),))
        if (kind == "extns"):
            return Item(kind, csr_write_words(
                CsrReg.EXTNS, self.random.randrange(1 << 4) & self.knobs.extns
            ))
        if (kind == "mpu"):
            return Item(kind, csr_write_words(CsrReg.MPUCF, mpu_region(
                self.byte(), self.random.randrange(8), self.random.randrange(MPU_REGION_COUNT),
                self.random.randrange(8)
            )))
        if (kind == "syscall"):
            return Item(kind, (syscall_inst(self.source()),))
        if (kind == "call"):
            return Item(kind, call_words(subroutine_address(
                self.random.randrange(self.knobs.call_depth)
            )))
        if (kind == "branch"):
            target: int = self.random.randrange(index + 1, length + 1)
            jump: int = self.random.choice((jal_inst, jmp_inst))(
                self.random.randrange(len(CmpFlag)), GpReg.RSCRATCH0, GpReg.RSCRATCH1
            )
            return Item(kind, address_words(PROGRAM_BASE) + (jump,), target = target)
        return self.fault(kind)


    def fault(self, kind: str) -> Item:
        if (kind == "illegal"):
            return Item(kind, (rrc_inst(self.register(), self.source(), reserved = 0x8),))
        if (kind == "overflow"):
            return Item(kind, address_words(0xFFFF)
                        + (load_inst(self.register(), ADDRESS_PAIR, 0x7),))
        if (kind == "vector"):
            return Item(kind, address_words(SYSCALL_VECTOR)
                        + (store_inst(self.source(), ADDRESS_PAIR, 0x0),))
        if (kind == "unmapped_vector"):
            return Item(kind, address_words(VECTOR_TABLE_LO)
                        + (store_inst(self.source(), ADDRESS_PAIR, 0x0),))
        if (kind == "unmapped"):
            return Item(kind, address_words(HOLE_UNMAPPED)
                        + (load_inst(self.register(), ADDRESS_PAIR, 0x0),))
        if (kind == "denied"):
            return Item(kind, address_words(HOLE_DENIED)
                        + (load_inst(self.register(), ADDRESS_PAIR, 0x0),))
        if (kind == "readonly"):
            return Item(kind, address_words(HOLE_READONLY)
                        + (store_inst(self.source(), ADDRESS_PAIR, 0x0),))
        if (kind == "privileged"):
            return Item(kind, (rsr_inst(CsrReg.CAUSE, self.register(), self.register()),))
        raise ValueError(f"'{kind}' is not an instruction class this generator can produce")


    def choose(self) -> str:
        weights: dict = self.knobs.weights()
        if (self.depth >= MAX_STACK_DEPTH):
            weights["push"] = 0
        if (self.depth <= 0):
            weights["pop"] = 0
        kinds: tuple = tuple(kind for kind, weight in weights.items() if (weight > 0))
        return self.random.choices(kinds, tuple(weights[kind] for kind in kinds))[0]


    def body(self, length: int) -> tuple:
        items: list = []
        for index in range(length):
            if (self.random.random() < self.knobs.fault_probability):
                items.append(self.fault(self.random.choice(self.knobs.faults())))
                continue
            items.append(self.item(self.choose(), index, length))
        return tuple(items)


    def prologue(self) -> tuple:
        words: list = list(csr_write_words(CsrReg.SADDR, STACK_RESET))
        words += [set_inst(GpReg.RZ, (DATA_BASE >> 8) & 0xFF),
                  set_inst(GpReg.RT, DATA_BASE & 0xFF)]
        words += [set_inst(register, self.byte()) for register in SEEDED_REGISTERS]
        if (self.knobs.user_mode):
            for region in USER_REGIONS:
                words += csr_write_words(CsrReg.MPUCF, region)
            words += csr_write_words(CsrReg.PRIVS, USER_MODE)
        return tuple(words)


    def subroutines(self) -> tuple:
        routines: list = []
        for index in range(self.knobs.call_depth):
            leaf: bool = (index + 1 >= self.knobs.call_depth)
            words: list = []
            if (not leaf):
                words += [rsr_inst(CsrReg.RADDR, GpReg.RSCRATCH0, GpReg.RSCRATCH1),
                          store_inst(GpReg.RSCRATCH1, STACK_PAIR, PUSH_OFFSET),
                          store_inst(GpReg.RSCRATCH0, STACK_PAIR, PUSH_OFFSET)]
            for _ in range(SUBROUTINE_LENGTH):
                words += list(self.item(self.random.choice(PLAIN_KINDS)).words)
            if (not leaf):
                words += list(call_words(subroutine_address(index + 1)))
                words += [load_inst(GpReg.RSCRATCH0, STACK_PAIR, POP_OFFSET),
                          load_inst(GpReg.RSCRATCH1, STACK_PAIR, POP_OFFSET),
                          wsr_inst(CsrReg.RADDR, GpReg.RSCRATCH0, GpReg.RSCRATCH1)]
            words.append(jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RADDR))
            routines.append(tuple(words))
        return tuple(routines)


def weighted_program(knobs: Knobs, seed: int) -> Program:
    builder: _Weighted = _Weighted(knobs, seed)
    program: Program = Program(
        knobs = knobs,
        seed = seed,
        prologue = builder.prologue(),
        body = builder.body(knobs.length),
        epilogue = (halt_inst(GpReg.RT),),
        subroutines = builder.subroutines(),
        memmap = knobs.memmap(),
        steps = 0
    )
    return replace(program, steps = 2 * program.length() + STEP_SLACK)


PROFILES: tuple = (
    Knobs("plain", "machine mode over a memory map which grants everything"),
    Knobs("faults", "machine mode with one instruction in twenty built to fault",
          fault_probability = 0.05),
    Knobs("holes", "a memory map with an unmapped, a denied and a read-only region",
          holes = True, fault_probability = 0.05),
    Knobs("user", "User Mode under an MPU which grants the program what it needs",
          user_mode = True),
    Knobs("userfaults", "User Mode against the holes, where privilege and mapping compete",
          user_mode = True, holes = True, fault_probability = 0.05),
    Knobs("calls", "subroutines two deep, saving and restoring RADDR through the stack",
          call_depth = 2),
    Knobs("syscalls", "system calls returning through RETSC", syscalls = True),
    Knobs("narrow", "the smallest processor: no extension but Dynamic Hardware",
          extns = Extn.DYNAMICHW),
    Knobs("dynamic", "a program which turns its own extensions off as it runs",
          call_depth = 1, syscalls = True, dynamic = True)
)
