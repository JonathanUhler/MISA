"""Shared widths and instruction-set encodings for the MISA core.

Every encoding here is copied verbatim from the ISA chapter and the misa-spec functional model and is
the single source of truth; other modules import these names and never redefine them.
"""

from amaranth.lib import enum


# Word widths (bits).
DATA_W: int = 8
ADDR_W: int = 16
INST_W: int = 16
SPEC_W: int = 16
HALF_W: int = 4
CMP_W: int = 3

# FLAGS bit positions.
FLAG_Z: int = 0
FLAG_C: int = 1
FLAG_N: int = 2
FLAG_V: int = 3

# EXTNS bit positions.
EXT_DYNAMICHW: int = 0
EXT_SYSCALL: int = 1
EXT_PRIVILEGE: int = 2
EXT_INTERRUPT: int = 3


class Opcode(enum.IntEnum, shape=4):
    HALT = 0x0  # also SYSCALL when bit 15 is set
    ADD = 0x1
    ADC = 0x2
    SUB = 0x3
    SBB = 0x4
    AND = 0x5
    OR = 0x6
    XOR = 0x7
    RRC = 0x8
    SET = 0x9
    LD = 0xA
    ST = 0xB
    RSR = 0xC
    WSR = 0xD
    JAL = 0xE
    JMP = 0xF  # also JMP-to-CSR when bit 15 is set


class Csr(enum.IntEnum, shape=4):
    SADDR = 0x1
    RADDR = 0x2
    FLAGS = 0x3
    CAUSE = 0x4
    EXTNS = 0x5
    RETSC = 0x8
    RETIR = 0x9
    PRIVS = 0xA


class Cmp(enum.IntEnum, shape=3):
    ALWAYS = 0b000
    EQUAL = 0b001
    NOT_EQUAL = 0b010
    GREATER = 0b011
    LESS = 0b100
    GREATER_EQUAL = 0b101
    LESS_EQUAL = 0b110


class AluOp(enum.IntEnum, shape=4):
    ADD = 0x1
    ADC = 0x2
    SUB = 0x3
    SBB = 0x4
    AND = 0x5
    OR = 0x6
    XOR = 0x7
    RRC = 0x8


class FaultKind(enum.IntEnum, shape=6):
    NOTHING = 0b000000
    HALT = 0b000001
    ILLEGAL = 0b001001
    PRIV_INSTRUCTION = 0b010001
    ADDR_OVERFLOW = 0b000010
    PC_OVERFLOW = 0b001010
    PRIV_ACCESS = 0b010010
    IMPROPER_RETURN = 0b000011
    COND_RETURN = 0b001011
    IMPROPER_SYSCALL = 0b010011
