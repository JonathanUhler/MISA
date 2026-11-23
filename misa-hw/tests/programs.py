from misa_hw.grammar import GpReg, Op

from memory import MEMORY_SIZE


HALT_WORD: int = 0x0000

VECTOR_TABLE_LO: int = 0xFFF0
INTERRUPT_VECTOR: int = 0xFFF8
SYSCALL_VECTOR: int = 0xFFFA
FAULT_VECTOR: int = 0xFFFC
RESET_VECTOR: int = 0xFFFE

PROGRAM_BASE: int = 0xC000
PAGE_SIZE: int = 0x0100

SYSCALL_BIT: int = 1 << 15
JMP_CSR_BIT: int = 1 << 15


def instruction(op: int, n1: int = 0, high: int = 0) -> int:
    return (op & 0xF) | ((n1 & 0xF) << 4) | ((high & 0xFF) << 8)


def nibbles(op: int, n1: int = 0, n2: int = 0, n3: int = 0) -> int:
    return instruction(op, n1, (n2 & 0xF) | ((n3 & 0xF) << 4))


def word_bytes(word: int) -> bytes:
    return bytes((word & 0xFF, (word >> 8) & 0xFF))


def encode(op: int, n1: int, rest: int = 0) -> bytes:
    return word_bytes(instruction(op, n1, rest))


def patched(image: bytes, addr: int, *words: bytes) -> bytes:
    buffer: bytearray = bytearray(image)
    for index, word in enumerate(words):
        buffer[addr + 2 * index:addr + 2 * index + 2] = word
    return bytes(buffer)


def alu_inst(op: int, rd: int, rs1: int, rs2: int) -> int:
    return nibbles(op, rd, rs1, rs2)


def rrc_inst(rd: int, rs: int, reserved: int = 0) -> int:
    return nibbles(Op.RRC, rd, rs, reserved)


def set_inst(rd: int, imm: int) -> int:
    return instruction(Op.SET, rd, imm)


def load_inst(rd: int, rsb: int, imm: int) -> int:
    return nibbles(Op.LD, rd, rsb, imm)


def store_inst(rs: int, rsb: int, imm: int) -> int:
    return nibbles(Op.ST, rs, rsb, imm)


def rsr_inst(csr: int, rs1: int, rs2: int) -> int:
    return nibbles(Op.RSR, rs1, rs2, csr)


def wsr_inst(csr: int, rs1: int, rs2: int) -> int:
    return nibbles(Op.WSR, rs1, rs2, csr)


def jal_inst(cmp_flag: int, rs1: int, rs2: int, reserved: int = 0) -> int:
    return nibbles(Op.JAL, rs1, rs2, (cmp_flag & 0b111) | ((reserved & 1) << 3))


def jmp_inst(cmp_flag: int, rs1: int, rs2: int) -> int:
    return nibbles(Op.JMP, rs1, rs2, cmp_flag & 0b111)


def jmp_csr_inst(cmp_flag: int, csr: int, reserved: int = 0) -> int:
    return nibbles(Op.JMP, csr, reserved, cmp_flag & 0b111) | JMP_CSR_BIT


def halt_inst(rs: int = GpReg.R0, reserved: int = 0) -> int:
    return instruction(Op.HALT, rs, reserved & 0x7F)


def syscall_inst(rs: int = GpReg.R0, reserved: int = 0) -> int:
    return halt_inst(rs, reserved) | SYSCALL_BIT


def mpu_region(base: int, size: int, index: int, caps: int) -> int:
    return ((base & 0xFF) << 8) | ((size & 0b111) << 5) | ((index & 0b11) << 3) | (caps & 0b111)


def mpu_pages(base: int, size: int) -> range:
    first: int = (base >> size) << size
    return range(first, first + (1 << size))


def csr_write_words(csr: int, value: int) -> tuple:
    return (
        set_inst(GpReg.RSCRATCH0, (value >> 8) & 0xFF),
        set_inst(GpReg.RSCRATCH1, value & 0xFF),
        wsr_inst(csr, GpReg.RSCRATCH0, GpReg.RSCRATCH1)
    )


def filled_memory(fill: int = HALT_WORD) -> bytearray:
    if ((fill & 0xFFFF) != fill):
        raise ValueError(f"fill word {fill:#x} is not 16 bits")
    return bytearray(word_bytes(fill) * (MEMORY_SIZE // 2))


def patterned_memory() -> bytearray:
    return bytearray((addr ^ (addr >> 8)) & 0xFF for addr in range(MEMORY_SIZE))


def block_image(blocks: dict, vectors: dict = None, memory: bytearray = None) -> bytes:
    image: bytearray = filled_memory() if (memory is None) else memory
    if (len(image) != MEMORY_SIZE):
        raise ValueError(f"the image is {len(image)} bytes, expected {MEMORY_SIZE}")

    for addr, words in blocks.items():
        if (addr % 2):
            raise ValueError(f"the block at {addr:#06x} is not instruction aligned")
        if (addr + 2 * len(words) > MEMORY_SIZE):
            raise ValueError(f"the block at {addr:#06x} does not fit in memory")
        for index, word in enumerate(words):
            image[addr + 2 * index:addr + 2 * index + 2] = word_bytes(word)

    for addr, target in (vectors or {}).items():
        if (not (VECTOR_TABLE_LO <= addr < MEMORY_SIZE)) or (addr % 2):
            raise ValueError(f"{addr:#06x} is not a vector of the reset vector table")
        image[addr:addr + 2] = word_bytes(target)

    return bytes(image)
