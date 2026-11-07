from amaranth.lib.enum import IntEnum


class Op(IntEnum, shape = 4):
    HALT = 0b0000
    ADD  = 0b0001
    ADC  = 0b0010
    SUB  = 0b0011
    SBB  = 0b0100
    AND  = 0b0101
    OR   = 0b0110
    XOR  = 0b0111
    RRC  = 0b1000
    SET  = 0b1001
    LD   = 0b1010
    ST   = 0b1011
    RSR  = 0b1100
    WSR  = 0b1101
    JAL  = 0b1110
    JMP  = 0b1111


class ArithmeticOp(IntEnum, shape = 4):
    ADD = Op.ADD
    ADC = Op.ADC
    SUB = Op.SUB
    SBB = Op.SBB
    AND = Op.AND
    OR  = Op.OR
    XOR = Op.XOR
    RRC = Op.RRC


class Cmp(IntEnum, shape = 3):
    ALWAYS        = 0b000
    EQUAL         = 0b001
    NOT_EQUAL     = 0b010
    GREATER       = 0b011
    LESS          = 0b100
    GREATER_EQUAL = 0b101
    LESS_EQUAL    = 0b110


class Csr(IntEnum, shape = 4):
    SADDR = 0b0001
    RADDR = 0b0010
    FLAGS = 0b0011
    CAUSE = 0b0100
    EXTNS = 0b0101
    RETSC = 0b1000
    RETIR = 0b1001
    PRIVS = 0b1010


class Extn(IntEnum, shape = 4):
    DYNAMICHW = 0b0001
    SYSCALL   = 0b0010
    PRIVILEGE = 0b0100
    INTERRUPT = 0b1000


class Reg(IntEnum, shape = 4):
    R0        = 0b0000
    RA        = 0b0001
    RB        = 0b0010
    RC        = 0b0011
    RD        = 0b0100
    RE        = 0b0101
    RF        = 0b0110
    RU        = 0b0111
    RV        = 0b1000
    RW        = 0b1001
    RX        = 0b1010
    RY        = 0b1011
    RZ        = 0b1100
    RT        = 0b1101
    RSCRATCH0 = 0b1110
    RSCRATCH1 = 0b1111


class Flag(IntEnum, shape = 4):
    Z = 0b0001
    C = 0b0010
    N = 0b0100
    V = 0b1000


class FaultKind(IntEnum, shape = 6):
    NOTHING          = 0b000000
    HALT             = 0b000001
    ILLEGAL          = 0b001001
    PRIV_INSTRUCTION = 0b010001
    ADDR_OVERFLOW    = 0b000010
    PC_OVERFLOW      = 0b001010
    PRIV_ACCESS      = 0b010010
    IMPROPER_RETURN  = 0b000011
    COND_RETURN      = 0b001011
    IMPROPER_SYSCALL = 0b010011

