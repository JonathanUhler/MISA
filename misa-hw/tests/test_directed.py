from importlib import import_module
import re
from types import ModuleType

from pytest import MarkDecorator, mark

from misa_hw.grammar import AccessKind, CmpFlag, CsrReg, Extn, FaultKind, GpReg, Op, OverflowKind

from compare import (
    Directed, EVERYTHING, describe_cause, expect_fault, expect_no_fault, handler_index, state_at
)
from generators import (
    DEFAULT_EXTNS, HANDLER_BASE, ILLEGAL_WORD, INTERRUPT_BASE, SYSCALL_BASE, USER_MODE,
    USER_MODE_WORDS, USER_PAGE_REGION, directed_image
)
from memory import (
    CAPS_READ, CAPS_RWX, CAPS_WRITE, MEMORY_SIZE, MemMap, memmap_hole, memmap_with
)
from programs import (
    FAULT_VECTOR, INTERRUPT_VECTOR, PAGE_SIZE, PROGRAM_BASE, RESET_VECTOR, SYSCALL_VECTOR,
    VECTOR_TABLE_LO, addr_of, alu_inst, cause_kind, cause_reserved, cause_reason, cause_status,
    cause_type, cause_word, csr_write_words, flags_word, halt_inst, jal_inst, jmp_csr_inst,
    jmp_inst, load_inst, mpu_pages, mpu_region, rrc_inst, rsr_inst, set_inst, store_inst,
    syscall_inst, wsr_inst
)
from state import (
    CSR_WRITE_MASKS, MisaState, RESERVED_CSRS, USER_READABLE_CSRS, USER_WRITABLE_CSRS
)
from target import RunResult


pytestmark: MarkDecorator = mark.subset

CATALOGUE: dict = {
    "DEC": 10, "ALU": 8, "MEM": 13, "FLT": 13, "VEC": 8,
    "CSR": 7, "SC": 11, "INT": 11, "PRV": 15, "DHW": 5, "RST": 5
}

CATALOGUE_MODULES: tuple = ("test_directed", "test_interrupts")
DEFERRED: dict = {}

TEST_ID: re.Pattern = re.compile(r"^test_([a-z]+)_(\d\d)_")

NO_DYNAMICHW: int = DEFAULT_EXTNS & ~Extn.DYNAMICHW
NO_SYSCALL: int = DEFAULT_EXTNS & ~Extn.SYSCALL
NO_PRIVILEGE: int = DEFAULT_EXTNS & ~Extn.PRIVILEGE
NO_INTERRUPT: int = DEFAULT_EXTNS & ~Extn.INTERRUPT

ALL_CSRS: tuple = tuple(CsrReg)
ALL_FLAGS: int = CSR_WRITE_MASKS["flags"]

SECOND_BASE: int = PROGRAM_BASE + PAGE_SIZE
THIRD_BASE: int = PROGRAM_BASE + 2 * PAGE_SIZE
DATA_BASE: int = 0x0200

HALT_CAUSE: int = cause_word(FaultKind.HALT)

PROGRAM_PAGE: int = PROGRAM_BASE >> 8


def catalogue_ids() -> set:
    return {
        f"{prefix}-{number:02d}"
        for prefix, count in CATALOGUE.items() for number in range(1, count + 1)
    }


def implemented_ids() -> set:
    found: set = set()
    for name in CATALOGUE_MODULES:
        module: ModuleType = import_module(name)
        for symbol in tuple(vars(module)):
            matched: re.Match = TEST_ID.match(symbol)
            if (matched):
                found.add(f"{matched.group(1).upper()}-{matched.group(2)}")
    return found


def test_the_catalogue_is_implemented_or_deferred() -> None:
    catalogue: set = catalogue_ids()
    implemented: set = implemented_ids()
    deferred: set = set(DEFERRED)

    assert implemented <= catalogue, \
        f"tests name cases the catalogue does not have: {sorted(implemented - catalogue)}"
    assert deferred <= catalogue, \
        f"DEFERRED names cases the catalogue does not have: {sorted(deferred - catalogue)}"
    assert not (implemented & deferred), \
        f"these cases are both implemented and deferred: {sorted(implemented & deferred)}"
    assert implemented | deferred == catalogue, (
        f"these catalogue cases have neither a test nor a reason to be deferred: "
        f"{sorted(catalogue - implemented - deferred)}"
    )


HALT_RESERVED_WORDS: tuple = (
    halt_inst(GpReg.R0, 0x01), halt_inst(GpReg.RA, 0x40), halt_inst(GpReg.RSCRATCH1, 0x7F),
    syscall_inst(GpReg.R0, 0x01), syscall_inst(GpReg.RA, 0x40),
    syscall_inst(GpReg.RSCRATCH1, 0x7F)
)
RESERVED_NIBBLES: tuple = (0x1, 0x8, 0xF)


@mark.parametrize("word", HALT_RESERVED_WORDS, ids = hex)
def test_dec_01_a_nonzero_halt_reserved_field_is_illegal(directed: Directed, word: int) -> None:
    states: list = directed.run(directed_image((word,)))
    expect_fault(states, FaultKind.ILLEGAL)


@mark.parametrize("reserved", RESERVED_NIBBLES, ids = hex)
def test_dec_02_a_nonzero_rrc_reserved_field_is_illegal(directed: Directed,
                                                        reserved: int) -> None:
    states: list = directed.run(directed_image((rrc_inst(GpReg.RA, GpReg.RB, reserved),)))
    expect_fault(states, FaultKind.ILLEGAL)


def test_dec_03_a_jal_with_the_target_bit_set_is_illegal(directed: Directed) -> None:
    word: int = jal_inst(CmpFlag.ALWAYS, GpReg.RA, GpReg.RB, reserved = 1)
    assert word & (1 << 15), "the case is a JAL whose bit 15 is set"

    states: list = directed.run(directed_image((word,)))
    expect_fault(states, FaultKind.ILLEGAL)


@mark.parametrize("reserved", RESERVED_NIBBLES, ids = hex)
def test_dec_04_a_nonzero_jmp_csr_reserved_field_is_illegal(directed: Directed,
                                                            reserved: int) -> None:
    word: int = jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.SADDR, reserved)
    states: list = directed.run(directed_image((word,)))
    expect_fault(states, FaultKind.ILLEGAL)


@mark.parametrize("csr", RESERVED_CSRS, ids = hex)
@mark.parametrize("named", ["rsr", "wsr", "jmp"])
def test_dec_05_a_reserved_special_register_encoding_is_illegal(directed: Directed, named: str,
                                                                csr: int) -> None:
    word: int = {
        "rsr": rsr_inst(csr, GpReg.RA, GpReg.RB),
        "wsr": wsr_inst(csr, GpReg.RA, GpReg.RB),
        "jmp": jmp_csr_inst(CmpFlag.ALWAYS, csr)
    }[named]

    states: list = directed.run(directed_image((word,)))
    expect_fault(states, FaultKind.ILLEGAL)


RESERVED_CMP: int = 0b111


@mark.parametrize("named", ["jal", "jmp", "jmp_csr"])
def test_dec_06_the_reserved_comparison_is_illegal(directed: Directed, named: str) -> None:
    word: int = {
        "jal": jal_inst(RESERVED_CMP, GpReg.RA, GpReg.RB),
        "jmp": jmp_inst(RESERVED_CMP, GpReg.RA, GpReg.RB),
        "jmp_csr": jmp_csr_inst(RESERVED_CMP, CsrReg.SADDR)
    }[named]

    states: list = directed.run(directed_image((word,)))
    expect_fault(states, FaultKind.ILLEGAL)


def test_dec_07_a_syscall_without_the_extension_is_illegal(directed: Directed) -> None:
    image: bytes = directed_image((syscall_inst(GpReg.RA),))
    states: list = directed.run(image, extns = NO_SYSCALL)
    expect_fault(states, FaultKind.ILLEGAL)


GATED_CSR_WORDS: tuple = (
    (rsr_inst(CsrReg.RETSC, GpReg.RA, GpReg.RB), NO_SYSCALL, "rsr-retsc"),
    (wsr_inst(CsrReg.RETSC, GpReg.RA, GpReg.RB), NO_SYSCALL, "wsr-retsc"),
    (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC), NO_SYSCALL, "jmp-retsc"),
    (rsr_inst(CsrReg.RETIR, GpReg.RA, GpReg.RB), NO_INTERRUPT, "rsr-retir"),
    (wsr_inst(CsrReg.RETIR, GpReg.RA, GpReg.RB), NO_INTERRUPT, "wsr-retir"),
    (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETIR), NO_INTERRUPT, "jmp-retir"),
    (rsr_inst(CsrReg.PRIVS, GpReg.RA, GpReg.RB), NO_PRIVILEGE, "rsr-privs"),
    (wsr_inst(CsrReg.PRIVS, GpReg.RA, GpReg.RB), NO_PRIVILEGE, "wsr-privs"),
    (rsr_inst(CsrReg.MPUCF, GpReg.RA, GpReg.RB), NO_PRIVILEGE, "rsr-mpucf"),
    (wsr_inst(CsrReg.MPUCF, GpReg.RA, GpReg.RB), NO_PRIVILEGE, "wsr-mpucf")
)


@mark.parametrize("word, extns, name", GATED_CSR_WORDS, ids = lambda case: case)
def test_dec_08_an_extension_gated_register_is_illegal_without_it(directed: Directed, word: int,
                                                                  extns: int, name: str) -> None:
    states: list = directed.run(directed_image((word,)), extns = extns)
    expect_fault(states, FaultKind.ILLEGAL)


def test_dec_09_legality_follows_the_enabled_extensions(directed: Directed) -> None:
    word: int = rsr_inst(CsrReg.RETSC, GpReg.RA, GpReg.RB)
    image: bytes = directed_image((word, halt_inst()))

    enabled: list = directed.run(image, extns = DEFAULT_EXTNS)
    assert expect_no_fault(enabled).halted, "the same word decodes with the extension enabled"
    assert enabled[-1].masked_csr("cause") == HALT_CAUSE

    disabled: list = directed.run(image, extns = NO_SYSCALL)
    expect_fault(disabled, FaultKind.ILLEGAL)


def test_dec_10_an_illegal_instruction_reports_itself_exactly(directed: Directed) -> None:
    words: tuple = (set_inst(GpReg.RA, 0x5A), rrc_inst(GpReg.RB, GpReg.RA, reserved = 0x1))
    states: list = directed.run(directed_image(words))
    entry: MisaState = expect_fault(states, FaultKind.ILLEGAL, at = addr_of(1))

    cause: int = entry.masked_csr("cause")
    assert cause_reason(cause) == 0b001, "the Reason field is Instruction usage"
    assert cause_type(cause) == 0b001, "the Type field is Illegal instruction"
    assert cause_status(cause) == 0x00, "Illegal instruction does not use the extended status"
    assert cause_reserved(cause) == 0b00, "the reserved bits of CAUSE read zero"
    assert entry.gp_regs[GpReg.RA] == 0x5A, "the instruction before the illegal one still ran"


def label(value: object) -> str:
    return value if (isinstance(value, str)) else None


def alu_program(op: int, flags_in: int, a: int, b: int, rd: int = GpReg.RC) -> tuple:
    return csr_write_words(CsrReg.FLAGS, flags_in) + (
        set_inst(GpReg.RA, a),
        set_inst(GpReg.RB, b),
        alu_inst(op, rd, GpReg.RA, GpReg.RB),
        halt_inst()
    )


ALU_CORNERS: tuple = (
    (Op.ADD, 0x0, 0x7F, 0x01, 0x80, (0, 0, 1, 1), "add-7f+01"),
    (Op.ADD, 0x0, 0x80, 0x80, 0x00, (1, 1, 0, 1), "add-80+80"),
    (Op.ADD, 0x0, 0xFF, 0x01, 0x00, (1, 1, 0, 0), "add-ff+01"),
    (Op.ADD, 0x0, 0x00, 0x00, 0x00, (1, 0, 0, 0), "add-00+00"),
    (Op.ADC, 0x2, 0x7F, 0x01, 0x81, (0, 0, 1, 1), "adc-7f+01+c"),
    (Op.ADC, 0x2, 0xFF, 0x00, 0x00, (1, 1, 0, 0), "adc-ff+00+c"),
    (Op.ADC, 0x0, 0x7F, 0x01, 0x80, (0, 0, 1, 1), "adc-7f+01"),
    (Op.SUB, 0x0, 0x00, 0x01, 0xFF, (0, 0, 1, 0), "sub-00-01"),
    (Op.SUB, 0x0, 0x80, 0x01, 0x7F, (0, 1, 0, 1), "sub-80-01"),
    (Op.SUB, 0x0, 0xFF, 0x01, 0xFE, (0, 1, 1, 0), "sub-ff-01"),
    (Op.SBB, 0x0, 0x00, 0x01, 0xFE, (0, 0, 1, 0), "sbb-00-01"),
    (Op.SBB, 0x2, 0x00, 0x01, 0xFF, (0, 0, 1, 0), "sbb-00-01+c"),
    (Op.SBB, 0x2, 0x80, 0x01, 0x7F, (0, 1, 0, 1), "sbb-80-01+c")
)


@mark.parametrize("op, flags_in, a, b, result, flags, name", ALU_CORNERS, ids = label)
def test_alu_01_the_arithmetic_corners_match_the_flag_table(directed: Directed, op: int,
                                                            flags_in: int, a: int, b: int,
                                                            result: int, flags: tuple,
                                                            name: str) -> None:
    states: list = directed.run(directed_image(alu_program(op, flags_in, a, b)))
    final: MisaState = expect_no_fault(states)
    expected: int = flags_word(*flags)

    assert final.halted, "the program ends in HALT"
    assert final.gp_regs[GpReg.RC] == result, (
        f"{Op(op).name} {a:#04x} {b:#04x} with FLAGS={flags_in:#03x} produced "
        f"{final.gp_regs[GpReg.RC]:#04x} where the manual requires {result:#04x}"
    )
    assert final.masked_csr("flags") == expected, (
        f"{Op(op).name} {a:#04x} {b:#04x} left FLAGS={final.masked_csr('flags'):#03x} where the "
        f"manual's table requires {expected:#03x} (z,c,n,v = {flags})"
    )


@mark.parametrize("a, b, carry", [(0x05, 0x03, 1), (0x03, 0x05, 0), (0x00, 0x00, 1)])
def test_alu_02_a_subtraction_carry_is_not_a_borrow(directed: Directed, a: int, b: int,
                                                    carry: int) -> None:
    states: list = directed.run(directed_image(alu_program(Op.SUB, 0x0, a, b)))
    final: MisaState = expect_no_fault(states)

    requirement: str = "needs no borrow and must set C" if (carry) else "borrows and must clear C"
    assert (final.masked_csr("flags") >> 1) & 1 == carry, \
        f"{a:#04x} - {b:#04x} {requirement}"


LOGIC_CASES: tuple = (
    (Op.AND, 0x80, 0x80, 0x80, (0, 0, 1, 0), "and-80&80"),
    (Op.AND, 0x0F, 0xF0, 0x00, (1, 0, 0, 0), "and-0f&f0"),
    (Op.OR, 0x80, 0x01, 0x81, (0, 0, 1, 0), "or-80|01"),
    (Op.OR, 0x00, 0x00, 0x00, (1, 0, 0, 0), "or-00|00"),
    (Op.XOR, 0x80, 0x80, 0x00, (1, 0, 0, 0), "xor-80^80"),
    (Op.XOR, 0xFF, 0x7F, 0x80, (0, 0, 1, 0), "xor-ff^7f")
)


@mark.parametrize("op, a, b, result, flags, name", LOGIC_CASES, ids = label)
def test_alu_03_the_logic_operations_always_clear_carry_and_overflow(directed: Directed, op: int,
                                                                     a: int, b: int, result: int,
                                                                     flags: tuple,
                                                                     name: str) -> None:
    states: list = directed.run(directed_image(alu_program(op, ALL_FLAGS, a, b)))
    final: MisaState = expect_no_fault(states)

    assert final.gp_regs[GpReg.RC] == result
    assert final.masked_csr("flags") == flags_word(*flags), (
        f"{Op(op).name} left FLAGS={final.masked_csr('flags'):#03x}, and the direct-set table "
        f"requires {flags_word(*flags):#03x} (z,c,n,v = {flags})"
    )


RRC_CASES: tuple = (
    (0x2, 0x01, 0x80, (0, 1, 1, 0), "c-in-01"),
    (0x2, 0x00, 0x80, (0, 0, 1, 0), "c-in-00"),
    (0xF, 0x02, 0x81, (0, 0, 1, 0), "all-flags-02"),
    (0x0, 0x02, 0x01, (0, 0, 0, 0), "no-c-02"),
    (0x0, 0x01, 0x00, (1, 1, 0, 0), "no-c-01")
)


@mark.parametrize("flags_in, rs, result, flags, name", RRC_CASES, ids = label)
def test_alu_04_rrc_rotates_through_carry_in_the_documented_order(directed: Directed,
                                                                  flags_in: int, rs: int,
                                                                  result: int, flags: tuple,
                                                                  name: str) -> None:
    words: tuple = csr_write_words(CsrReg.FLAGS, flags_in) + (
        set_inst(GpReg.RA, rs),
        rrc_inst(GpReg.RC, GpReg.RA),
        halt_inst()
    )
    states: list = directed.run(directed_image(words))
    final: MisaState = expect_no_fault(states)

    assert final.gp_regs[GpReg.RC] == result, (
        f"RRC of {rs:#04x} with FLAGS={flags_in:#03x} produced "
        f"{final.gp_regs[GpReg.RC]:#04x}; bit 7 takes the old C and bit 0 is shifted out"
    )
    assert final.masked_csr("flags") == flags_word(*flags), (
        f"RRC left FLAGS={final.masked_csr('flags'):#03x} where the manual requires "
        f"{flags_word(*flags):#03x}: the flags update from the result and C is then overwritten "
        f"with bit 0 of RS"
    )


def test_alu_05_a_result_discarded_into_r0_still_sets_the_flags(directed: Directed) -> None:
    states: list = directed.run(
        directed_image(alu_program(Op.ADD, 0x0, 0x7F, 0x01, rd = GpReg.R0))
    )
    final: MisaState = expect_no_fault(states)

    assert final.gp_regs[GpReg.R0] == 0x00, "R0 discards the result"
    assert final.masked_csr("flags") == flags_word(0, 0, 1, 1), \
        "the Flag Results section is explicit that the flags are still set"


ALIASED_OPERAND: int = 0x81
ALIASED_RESULTS: tuple = (
    (Op.ADD, 0x02, "add"), (Op.ADC, 0x02, "adc"), (Op.SUB, 0x00, "sub"), (Op.SBB, 0xFF, "sbb"),
    (Op.AND, 0x81, "and"), (Op.OR, 0x81, "or"), (Op.XOR, 0x00, "xor"), (Op.RRC, 0x40, "rrc")
)


@mark.parametrize("op, result, name", ALIASED_RESULTS, ids = label)
def test_alu_06_an_operation_reads_its_operands_before_writing_them(directed: Directed, op: int,
                                                                    result: int,
                                                                    name: str) -> None:
    operation: int = rrc_inst(GpReg.RA, GpReg.RA) if (op == Op.RRC) \
        else alu_inst(op, GpReg.RA, GpReg.RA, GpReg.RA)
    words: tuple = (set_inst(GpReg.RA, ALIASED_OPERAND), operation, halt_inst())

    states: list = directed.run(directed_image(words))
    final: MisaState = expect_no_fault(states)

    assert final.gp_regs[GpReg.RA] == result, (
        f"{Op(op).name} RA RA RA with RA={ALIASED_OPERAND:#04x} produced "
        f"{final.gp_regs[GpReg.RA]:#04x} where reading before writing gives {result:#04x}"
    )


DATUM: int = 0xA5


def test_alu_07_the_instructions_which_must_not_touch_flags_do_not(directed: Directed) -> None:
    words: tuple = csr_write_words(CsrReg.FLAGS, ALL_FLAGS) + (
        set_inst(GpReg.RA, SECOND_BASE >> 8),
        set_inst(GpReg.RB, SECOND_BASE & 0xFF),
        set_inst(GpReg.RC, 0x40),
        set_inst(GpReg.RD, DATA_BASE >> 8),
        set_inst(GpReg.RE, DATA_BASE & 0xFF),
        load_inst(GpReg.RU, GpReg.RD, 0),
        store_inst(GpReg.RU, GpReg.RD, 0),
        rsr_inst(CsrReg.SADDR, GpReg.RV, GpReg.RW),
        wsr_inst(CsrReg.RADDR, GpReg.RV, GpReg.RW),
        jal_inst(CmpFlag.ALWAYS, GpReg.RA, GpReg.RB)
    )
    image: bytes = directed_image(
        words,
        blocks = {SECOND_BASE: (jmp_inst(CmpFlag.ALWAYS, GpReg.RA, GpReg.RC),)},
        data = {DATA_BASE: bytes((DATUM,))}
    )
    states: list = directed.run(image)
    final: MisaState = expect_no_fault(states)

    assert final.halted and final.pc == SECOND_BASE + 0x40, "the JAL and the JMP were both taken"
    assert final.gp_regs[GpReg.RU] == DATUM, "the LD read the datum it was pointed at"
    assert final.csrs["raddr"] == addr_of(len(words)), "the JAL linked over the WSR's RADDR"
    assert {state.masked_csr("flags") for state in states} == {0x0, ALL_FLAGS}, (
        "only the WSR to FLAGS may change FLAGS; HALT, SET, LD, ST, RSR, WSR, JAL and JMP all "
        "have to leave it alone"
    )


@mark.parametrize("flags_in, taken", [(flags_word(1, 0, 0, 0), True), (0x0, False)],
                  ids = ["equal", "not-equal"])
def test_alu_08_a_comparison_observes_a_write_to_flags(directed: Directed, flags_in: int,
                                                       taken: bool) -> None:
    words: tuple = csr_write_words(CsrReg.FLAGS, flags_in) + (
        set_inst(GpReg.RA, SECOND_BASE >> 8),
        set_inst(GpReg.RB, SECOND_BASE & 0xFF),
        jal_inst(CmpFlag.EQUAL, GpReg.RA, GpReg.RB)
    )
    states: list = directed.run(directed_image(words))
    final: MisaState = expect_no_fault(states)

    assert final.halted
    assert final.pc == (SECOND_BASE if (taken) else addr_of(len(words))), (
        f"with FLAGS={flags_in:#03x} the EQUAL branch should "
        f"{'have been taken' if (taken) else 'not have been taken'}"
    )
    assert final.masked_csr("flags") == flags_in, "the branch does not disturb FLAGS"


MEM_PATTERN: bytes = bytes(0x30 + index for index in range(0x20))
MEM_BASE: int = DATA_BASE + 0x10
MEM_DATA: dict = {DATA_BASE: MEM_PATTERN}

STACK_TOP: int = 0x0300
STACK_DATA: dict = {STACK_TOP - 1: bytes((0x11,)), STACK_TOP: bytes((0x22,))}

STORED: int = 0x5A
OFFSETS: tuple = ((0x8, -8), (0xF, -1), (0x0, 0), (0x7, 7))


def expect_vector_table_intact(before: MisaState, after: MisaState) -> None:
    assert after.memory[VECTOR_TABLE_LO:] == before.memory[VECTOR_TABLE_LO:], \
        "the reset vector table must never be writable by any software"


@mark.parametrize("imm, offset", OFFSETS, ids = label)
@mark.parametrize("kind", ["ld", "st"])
def test_mem_01_a_register_base_offset_is_sign_extended(directed: Directed, kind: str, imm: int,
                                                        offset: int) -> None:
    loading: bool = kind == "ld"
    words: tuple = (
        set_inst(GpReg.RA, MEM_BASE >> 8),
        set_inst(GpReg.RB, MEM_BASE & 0xFF),
        set_inst(GpReg.RC, STORED),
        load_inst(GpReg.RC, GpReg.RA, imm) if (loading)
        else store_inst(GpReg.RC, GpReg.RA, imm),
        halt_inst()
    )
    states: list = directed.run(directed_image(words, data = MEM_DATA))
    final: MisaState = expect_no_fault(states)
    addr: int = MEM_BASE + offset

    if (loading):
        assert final.gp_regs[GpReg.RC] == MEM_PATTERN[addr - DATA_BASE], (
            f"LD with IMM={imm:#x} read {final.gp_regs[GpReg.RC]:#04x}, and the byte at "
            f"{MEM_BASE:#06x}{offset:+d} = {addr:#06x} is "
            f"{MEM_PATTERN[addr - DATA_BASE]:#04x}"
        )
    else:
        assert final.memory[addr] == STORED, \
            f"ST with IMM={imm:#x} did not write {addr:#06x} = {MEM_BASE:#06x}{offset:+d}"
        written: list = [
            byte for byte in range(DATA_BASE, DATA_BASE + len(MEM_PATTERN))
            if (final.memory[byte] != MEM_PATTERN[byte - DATA_BASE])
        ]
        assert written == [addr], f"the store also changed {[f'{a:#06x}' for a in written]}"


ACCESS_KINDS: tuple = (("ld", OverflowKind.READ), ("st", OverflowKind.WRITE))


def overflowing_access(directed: Directed, kind: str, base: int, imm: int, status: int) -> None:
    words: tuple = (
        set_inst(GpReg.RA, base >> 8),
        set_inst(GpReg.RB, base & 0xFF),
        load_inst(GpReg.RC, GpReg.RA, imm) if (kind == "ld")
        else store_inst(GpReg.RC, GpReg.RA, imm),
        halt_inst()
    )
    image: bytes = directed_image(words, data = MEM_DATA)
    states: list = directed.run(image)
    entry: MisaState = expect_fault(states, FaultKind.ADDR_OVERFLOW, status, at = addr_of(2))

    assert entry.memory == states[0].memory, \
        "an overflowing computation produces no address, so nothing can be written"
    assert not directed.journal(image, steps = handler_index(states)).bfm.writes, \
        "the refused access must not reach the memory model either"


@mark.parametrize("kind, status", ACCESS_KINDS, ids = label)
def test_mem_02_an_effective_address_past_the_top_overflows(directed: Directed, kind: str,
                                                            status: int) -> None:
    overflowing_access(directed, kind, 0xFFFF, 0x1, status)


@mark.parametrize("kind, status", ACCESS_KINDS, ids = label)
def test_mem_03_an_effective_address_below_zero_overflows(directed: Directed, kind: str,
                                                          status: int) -> None:
    overflowing_access(directed, kind, 0x0000, 0xF, status)


ZERO_PAIR_ADDR: int = 0x0040
ZERO_PAIR_DATUM: int = 0x6B


def test_mem_04_the_lowest_base_pair_takes_its_high_byte_from_r0(directed: Directed) -> None:
    words: tuple = (
        set_inst(GpReg.RA, ZERO_PAIR_ADDR & 0xFF),
        load_inst(GpReg.RC, GpReg.R0, 0),
        halt_inst()
    )
    image: bytes = directed_image(words, data = {ZERO_PAIR_ADDR: bytes((ZERO_PAIR_DATUM,))})
    final: MisaState = expect_no_fault(directed.run(image))

    assert final.gp_regs[GpReg.RC] == ZERO_PAIR_DATUM, (
        f"the load read {final.gp_regs[GpReg.RC]:#04x}, and the byte at "
        f"{ZERO_PAIR_ADDR:#06x} = 0x00 || RA is {ZERO_PAIR_DATUM:#04x}"
    )


SCRATCH_PAIR_ADDR: int = DATA_BASE + 0x30
SCRATCH_PAIR_DATUM: int = 0x99


def test_mem_05_the_highest_base_pair_wraps_within_four_bits(directed: Directed) -> None:
    words: tuple = (
        set_inst(GpReg.RSCRATCH0, SCRATCH_PAIR_ADDR >> 8),
        set_inst(GpReg.RSCRATCH1, SCRATCH_PAIR_ADDR & 0xFF),
        load_inst(GpReg.RC, GpReg.RSCRATCH0, 0),
        halt_inst()
    )
    image: bytes = directed_image(words, data = {SCRATCH_PAIR_ADDR: bytes((SCRATCH_PAIR_DATUM,))})
    final: MisaState = expect_no_fault(directed.run(image))

    assert final.gp_regs[GpReg.RC] == SCRATCH_PAIR_DATUM, \
        f"the load read {final.gp_regs[GpReg.RC]:#04x} rather than the byte at the pair's address"


ALIASED_BASE: int = DATA_BASE + 0x40
ALIASED_DATUM: int = 0x77


@mark.parametrize("kind", ["ld", "st"])
def test_mem_06_an_access_reads_its_base_pair_before_writing_it(directed: Directed,
                                                                kind: str) -> None:
    words: tuple = (
        set_inst(GpReg.RA, ALIASED_BASE >> 8),
        set_inst(GpReg.RB, ALIASED_BASE & 0xFF),
        load_inst(GpReg.RA, GpReg.RA, 0) if (kind == "ld")
        else store_inst(GpReg.RA, GpReg.RA, 0),
        halt_inst()
    )
    image: bytes = directed_image(words, data = {ALIASED_BASE: bytes((ALIASED_DATUM,))})
    final: MisaState = expect_no_fault(directed.run(image))

    assert final.gp_regs[GpReg.RB] == (ALIASED_BASE & 0xFF), "the low register is untouched"
    if (kind == "ld"):
        assert final.gp_regs[GpReg.RA] == ALIASED_DATUM, "the load lands in RA"
        assert final.memory[ALIASED_BASE] == ALIASED_DATUM, "a load writes no memory"
    else:
        assert final.memory[ALIASED_BASE] == (ALIASED_BASE >> 8), \
            "the store wrote the value RA held before the instruction"


def stack_words(saddr: int, operation: tuple) -> tuple:
    return csr_write_words(CsrReg.SADDR, saddr) + operation + (halt_inst(),)


def test_mem_07_a_stack_load_updates_saddr_before_it_reads(directed: Directed) -> None:
    words: tuple = stack_words(STACK_TOP, (load_inst(GpReg.RC, GpReg.RSCRATCH1, 0xF),))
    final: MisaState = expect_no_fault(
        directed.run(directed_image(words, data = STACK_DATA))
    )

    assert final.csrs["saddr"] == STACK_TOP - 1, "a stack load updates SADDR by the offset"
    assert final.gp_regs[GpReg.RC] == STACK_DATA[STACK_TOP - 1][0], (
        f"the load read {final.gp_regs[GpReg.RC]:#04x}; a stack load reads the updated SADDR "
        f"{STACK_TOP - 1:#06x}, not the value it started from"
    )


def test_mem_08_a_stack_store_writes_before_it_updates_saddr(directed: Directed) -> None:
    words: tuple = stack_words(
        STACK_TOP, (set_inst(GpReg.RC, STORED), store_inst(GpReg.RC, GpReg.RSCRATCH1, 0xF))
    )
    final: MisaState = expect_no_fault(
        directed.run(directed_image(words, data = STACK_DATA))
    )

    assert final.csrs["saddr"] == STACK_TOP - 1, "a stack store updates SADDR afterwards"
    assert final.memory[STACK_TOP] == STORED, \
        f"a stack store writes the current SADDR {STACK_TOP:#06x}"
    assert final.memory[STACK_TOP - 1] == STACK_DATA[STACK_TOP - 1][0], \
        "the address SADDR was updated to must not have been written"


VECTOR_PUSH_BELOW: int = VECTOR_TABLE_LO - 1


def test_mem_09_a_push_below_the_table_succeeds_however_saddr_ends_up(directed: Directed) -> None:
    words: tuple = stack_words(
        VECTOR_PUSH_BELOW, (set_inst(GpReg.RC, STORED), store_inst(GpReg.RC, GpReg.RSCRATCH1, 0x1))
    )
    states: list = directed.run(directed_image(words))
    final: MisaState = expect_no_fault(states)

    assert final.memory[VECTOR_PUSH_BELOW] == STORED, (
        f"the store writes {VECTOR_PUSH_BELOW:#06x}, which is outside the table, so it must be "
        f"performed even though SADDR ends up inside it"
    )
    assert final.csrs["saddr"] == VECTOR_TABLE_LO, "SADDR still takes the offset"
    expect_vector_table_intact(states[0], final)


def test_mem_10_a_push_inside_the_table_faults_however_saddr_would_end_up(
        directed: Directed) -> None:
    words: tuple = stack_words(
        VECTOR_TABLE_LO, (set_inst(GpReg.RC, STORED), store_inst(GpReg.RC, GpReg.RSCRATCH1, 0xF))
    )
    image: bytes = directed_image(words)
    states: list = directed.run(image)
    status: int = AccessKind.WRITE | ((VECTOR_TABLE_LO & 0xF) << 4)
    entry: MisaState = expect_fault(states, FaultKind.VECTOR_WRITE, status, at = addr_of(4))

    assert entry.csrs["saddr"] == VECTOR_TABLE_LO, (
        "a refused store must not update SADDR, even though the updated value would have been "
        "outside the table"
    )
    expect_vector_table_intact(states[0], entry)
    assert not directed.journal(image, steps = handler_index(states)).bfm.writes


STACK_LIMITS: tuple = (
    ("ld", 0xFFFF, 0x1, "ld-over"), ("ld", 0x0000, 0xF, "ld-under"),
    ("st", 0xFFFF, 0x1, "st-over"), ("st", 0x0000, 0xF, "st-under")
)


@mark.parametrize("kind, saddr, imm, name", STACK_LIMITS, ids = label)
def test_mem_11_a_stack_update_which_leaves_saddr_overflows(directed: Directed, kind: str,
                                                            saddr: int, imm: int,
                                                            name: str) -> None:
    operation: tuple = (load_inst(GpReg.RC, GpReg.RSCRATCH1, imm),) if (kind == "ld") \
        else (set_inst(GpReg.RC, STORED), store_inst(GpReg.RC, GpReg.RSCRATCH1, imm))
    words: tuple = stack_words(saddr, operation)

    states: list = directed.run(directed_image(words))
    entry: MisaState = expect_fault(states, FaultKind.ADDR_OVERFLOW, OverflowKind.SADDR,
                                    at = addr_of(2 + len(operation)))

    assert entry.csrs["saddr"] == saddr, "an overflowing stack operation leaves SADDR alone"
    assert entry.memory == states[0].memory, "and writes nothing"


REFUSED_PAGE: int = 0x0400


def test_mem_12_a_refused_stack_store_touches_neither_memory_nor_saddr(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(REFUSED_PAGE, REFUSED_PAGE + PAGE_SIZE - 1, "rx")
    words: tuple = stack_words(
        REFUSED_PAGE, (set_inst(GpReg.RC, STORED), store_inst(GpReg.RC, GpReg.RSCRATCH1, 0xF))
    )
    image: bytes = directed_image(words)

    states: list = directed.run(image, memmap)
    entry: MisaState = expect_fault(states, FaultKind.INVALID_ACCESS, AccessKind.WRITE,
                                    at = addr_of(4))

    assert entry.csrs["saddr"] == REFUSED_PAGE, "SADDR is not updated by a refused store"
    assert entry.memory == states[0].memory, "and no byte of memory changed"
    assert not directed.journal(image, memmap, steps = handler_index(states)).bfm.writes, \
        "the testbench must not perform a write the core refused"


@mark.parametrize("nibble", range(0x10), ids = hex)
def test_mem_13_a_register_base_store_into_the_table_is_a_vector_write(directed: Directed,
                                                                       nibble: int) -> None:
    addr: int = VECTOR_TABLE_LO | nibble
    words: tuple = (
        set_inst(GpReg.RA, addr >> 8),
        set_inst(GpReg.RB, addr & 0xFF),
        set_inst(GpReg.RC, STORED),
        store_inst(GpReg.RC, GpReg.RA, 0),
        halt_inst()
    )
    image: bytes = directed_image(words)
    states: list = directed.run(image)
    status: int = AccessKind.WRITE | (nibble << 4)
    entry: MisaState = expect_fault(states, FaultKind.VECTOR_WRITE, status, at = addr_of(3))

    assert cause_status(entry.masked_csr("cause")) >> 4 == nibble, \
        "the top nibble of the extended status is the low nibble of the refused address"
    expect_vector_table_intact(states[0], entry)
    assert not directed.journal(image, steps = handler_index(states)).bfm.writes


TOP_WORD: int = MEMORY_SIZE - 1
UNMAPPED_PAGE: int = 0x8000


def jump_words(target: int) -> tuple:
    return (
        set_inst(GpReg.RA, target >> 8),
        set_inst(GpReg.RB, target & 0xFF),
        jmp_inst(CmpFlag.ALWAYS, GpReg.RA, GpReg.RB)
    )


def test_flt_01_pc_overflow_outranks_a_fetch_fault_at_the_same_address(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(RESET_VECTOR, TOP_WORD, "r")
    states: list = directed.run(directed_image(jump_words(TOP_WORD)), memmap)

    expect_fault(states, FaultKind.PC_OVERFLOW, at = TOP_WORD)


def test_flt_02_a_sequential_overflow_keeps_the_instruction_it_completed(
        directed: Directed) -> None:
    retained: int = set_inst(GpReg.RA, 0x5A)
    image: bytes = directed_image((halt_inst(),), vectors = {RESET_VECTOR: retained})
    state: MisaState = directed.seed(image, pc = RESET_VECTOR)

    states: list = directed.resume(state, image, steps = 1)
    entry: MisaState = expect_fault(states, FaultKind.PC_OVERFLOW, at = RESET_VECTOR)

    assert entry.gp_regs[GpReg.RA] == 0x5A, (
        "a PC overflow raised by a sequential increment is detected after the instruction "
        "completed, so the manual requires its effects to be retained"
    )


def test_flt_03_address_overflow_outranks_the_unmapped_address_it_wraps_to(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(0x0000, PAGE_SIZE - 1)
    overflowing: tuple = (
        set_inst(GpReg.RA, 0xFF), set_inst(GpReg.RB, 0xFF),
        load_inst(GpReg.RC, GpReg.RA, 0x1), halt_inst()
    )
    plain: tuple = (
        set_inst(GpReg.RA, 0x00), set_inst(GpReg.RB, 0x00),
        load_inst(GpReg.RC, GpReg.RA, 0x0), halt_inst()
    )

    expect_fault(directed.run(directed_image(overflowing), memmap),
                 FaultKind.ADDR_OVERFLOW, OverflowKind.READ, at = addr_of(2))
    expect_fault(directed.run(directed_image(plain), memmap),
                 FaultKind.UNMAPPED_ADDRESS, AccessKind.READ, at = addr_of(2))


def test_flt_04_privileged_access_outranks_an_unmapped_address(directed: Directed) -> None:
    memmap: MemMap = memmap_hole(0x0000, PAGE_SIZE - 1)
    access: tuple = (
        set_inst(GpReg.RA, 0x00), set_inst(GpReg.RB, 0x00),
        load_inst(GpReg.RC, GpReg.RA, 0x0), halt_inst()
    )

    user: list = directed.run(directed_image(USER_MODE_WORDS + access), memmap)
    expect_fault(user, FaultKind.PRIV_ACCESS, AccessKind.READ,
                 at = addr_of(len(USER_MODE_WORDS) + 2))

    machine: list = directed.run(directed_image(access), memmap)
    expect_fault(machine, FaultKind.UNMAPPED_ADDRESS, AccessKind.READ, at = addr_of(2))


def vector_store_words(addr: int) -> tuple:
    return (
        set_inst(GpReg.RA, addr >> 8),
        set_inst(GpReg.RB, addr & 0xFF),
        set_inst(GpReg.RC, STORED),
        store_inst(GpReg.RC, GpReg.RA, 0),
        halt_inst()
    )


def test_flt_05_an_unmapped_address_outranks_a_vector_write(directed: Directed) -> None:
    addr: int = VECTOR_TABLE_LO + 2
    memmap: MemMap = memmap_hole(addr, addr + 1)
    states: list = directed.run(directed_image(vector_store_words(addr)), memmap)

    expect_fault(states, FaultKind.UNMAPPED_ADDRESS, AccessKind.WRITE, at = addr_of(3))


def test_flt_06_a_vector_write_outranks_an_invalid_access(directed: Directed) -> None:
    addr: int = VECTOR_TABLE_LO + 4
    memmap: MemMap = memmap_hole(VECTOR_TABLE_LO, TOP_WORD, "r")
    states: list = directed.run(directed_image(vector_store_words(addr)), memmap)
    status: int = AccessKind.WRITE | ((addr & 0xF) << 4)

    expect_fault(states, FaultKind.VECTOR_WRITE, status, at = addr_of(3))


FETCH_FAULTS: tuple = (
    (None, FaultKind.UNMAPPED_ADDRESS, "unmapped"),
    ("rw", FaultKind.INVALID_ACCESS, "not-executable")
)


def test_flt_07_a_fetch_from_unmapped_memory_is_an_unmapped_address(directed: Directed) -> None:
    memmap: MemMap = memmap_hole(UNMAPPED_PAGE, UNMAPPED_PAGE + PAGE_SIZE - 1)
    states: list = directed.run(directed_image(jump_words(UNMAPPED_PAGE)), memmap)

    expect_fault(states, FaultKind.UNMAPPED_ADDRESS, AccessKind.EXECUTE, at = UNMAPPED_PAGE)


def test_flt_08_a_fetch_from_non_executable_memory_is_an_invalid_access(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(UNMAPPED_PAGE, UNMAPPED_PAGE + PAGE_SIZE - 1, "rw")
    states: list = directed.run(directed_image(jump_words(UNMAPPED_PAGE)), memmap)

    expect_fault(states, FaultKind.INVALID_ACCESS, AccessKind.EXECUTE, at = UNMAPPED_PAGE)


@mark.parametrize("caps, kind, name", FETCH_FAULTS, ids = label)
def test_flt_09_the_high_fetch_byte_reports_its_own_fault(directed: Directed, caps: str,
                                                          kind: int, name: str) -> None:
    memmap: MemMap = memmap_with(((UNMAPPED_PAGE + 1, UNMAPPED_PAGE + 1, caps),))
    states: list = directed.run(directed_image(jump_words(UNMAPPED_PAGE)), memmap)

    expect_fault(states, kind, AccessKind.EXECUTE, at = UNMAPPED_PAGE)


SPLIT_FETCH_FAULTS: tuple = (
    (None, "rw", FaultKind.UNMAPPED_ADDRESS, "unmapped-then-invalid"),
    ("rw", None, FaultKind.INVALID_ACCESS, "invalid-then-unmapped")
)


@mark.parametrize("low, high, kind, name", SPLIT_FETCH_FAULTS, ids = label)
def test_flt_10_the_low_fetch_byte_decides_when_both_fault(directed: Directed, low: str,
                                                           high: str, kind: int,
                                                           name: str) -> None:
    memmap: MemMap = memmap_with((
        (UNMAPPED_PAGE, UNMAPPED_PAGE, low), (UNMAPPED_PAGE + 1, UNMAPPED_PAGE + 1, high)
    ))
    states: list = directed.run(directed_image(jump_words(UNMAPPED_PAGE)), memmap)

    expect_fault(states, kind, AccessKind.EXECUTE, at = UNMAPPED_PAGE)


def test_flt_11_an_invalid_access_is_raised_without_the_privilege_extension(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(REFUSED_PAGE, REFUSED_PAGE + PAGE_SIZE - 1, "rx")
    words: tuple = (
        set_inst(GpReg.RA, REFUSED_PAGE >> 8),
        set_inst(GpReg.RB, REFUSED_PAGE & 0xFF),
        set_inst(GpReg.RC, STORED),
        store_inst(GpReg.RC, GpReg.RA, 0),
        halt_inst()
    )
    states: list = directed.run(directed_image(words), memmap, extns = NO_PRIVILEGE)

    expect_fault(states, FaultKind.INVALID_ACCESS, AccessKind.WRITE, at = addr_of(3))


FAULT_PRESERVED: tuple = ("cause", "fault", "privs", "pc")
UNCHANGED_CASES: tuple = (
    ((ILLEGAL_WORD,), EVERYTHING, DEFAULT_EXTNS, "illegal"),
    ((set_inst(GpReg.RA, 0xFF), set_inst(GpReg.RB, 0xFF),
      load_inst(GpReg.RC, GpReg.RA, 0x1)), EVERYTHING, DEFAULT_EXTNS, "address-overflow"),
    (vector_store_words(VECTOR_TABLE_LO), EVERYTHING, DEFAULT_EXTNS, "vector-write"),
    (USER_MODE_WORDS + (halt_inst(),), EVERYTHING, DEFAULT_EXTNS, "privileged-instruction"),
    ((set_inst(GpReg.RA, 0x00), set_inst(GpReg.RB, 0x00),
      load_inst(GpReg.RC, GpReg.RA, 0x0)), memmap_hole(0x0000, PAGE_SIZE - 1),
     DEFAULT_EXTNS, "unmapped-load")
)


@mark.parametrize("words, memmap, extns, name", UNCHANGED_CASES, ids = label)
def test_flt_12_a_fault_changes_only_cause_fault_privs_and_the_program_counter(
        directed: Directed, words: tuple, memmap: MemMap, extns: int, name: str) -> None:
    states: list = directed.run(directed_image(words), memmap, extns = extns)
    index: int = handler_index(states)
    changed: set = {
        field for field, _, _ in states[index - 1].diff(states[index])
    }

    assert changed <= set(FAULT_PRESERVED), (
        f"entering the fault handler changed {sorted(changed - set(FAULT_PRESERVED))}, and the "
        f"manual allows a fault to change only {', '.join(FAULT_PRESERVED)}"
    )
    assert "cause" in changed and "pc" in changed, "the fault was supposed to be reported"


def test_flt_13_a_fault_inside_a_system_call_context_stays_in_it(directed: Directed) -> None:
    image: bytes = directed_image(
        (syscall_inst(GpReg.R0),), blocks = {SYSCALL_BASE: (ILLEGAL_WORD,)}
    )
    states: list = directed.run(image)
    entry: MisaState = expect_fault(states, FaultKind.ILLEGAL, at = SYSCALL_BASE)

    assert entry.in_syscall, \
        "a fault must not change whether the processor is in a system call context"
    assert not entry.in_interrupt


def test_flt_13_a_fault_inside_an_interrupt_context_stays_in_it(directed: Directed) -> None:
    image: bytes = directed_image((ILLEGAL_WORD,))
    state: MisaState = directed.seed(image, in_interrupt = True)

    states: list = directed.resume(state, image, steps = 1)
    entry: MisaState = expect_fault(states, FaultKind.ILLEGAL)

    assert entry.in_interrupt, (
        "being in an interrupt context is what masks interrupts, so a fault taken in one has to "
        "leave the fault handler masked as well"
    )


CAUSE_UNMAPPED_READ: int = cause_word(FaultKind.UNMAPPED_ADDRESS, AccessKind.READ)
CAUSE_INVALID_READ: int = cause_word(FaultKind.INVALID_ACCESS, AccessKind.READ)

VECTOR_READS: tuple = (
    (None, "w", FaultKind.UNMAPPED_ADDRESS, "unmapped-low"),
    ("w", None, FaultKind.INVALID_ACCESS, "unreadable-low")
)


def test_vec_01_an_unreadable_reset_vector_starts_in_the_fault_handler(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(RESET_VECTOR, TOP_WORD)
    states: list = directed.run(directed_image((halt_inst(),)), memmap)

    assert states[0].pc == HANDLER_BASE, \
        "execution must begin in the fault handler rather than at the reset handler"
    expect_fault(states, FaultKind.UNMAPPED_ADDRESS, AccessKind.READ, at = RESET_VECTOR)
    assert states[-1].halted, "the fault handler of this image is a HALT"


def test_vec_02_an_unreadable_fault_vector_at_reset_halts_without_executing(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(FAULT_VECTOR, TOP_WORD)
    image: bytes = directed_image((halt_inst(),))
    states: list = directed.run(image, memmap)

    assert len(states) == 1, \
        f"neither side can retire an instruction, but the run produced {len(states)} states"
    assert states[0].halted, "the processor must halt without executing any instruction"
    assert states[0].csrs["fault"] == FAULT_VECTOR, \
        "CAUSE and FAULT describe the failed fault vector read"
    assert states[0].masked_csr("cause") == CAUSE_UNMAPPED_READ
    assert not directed.journal(image, memmap).bfm.accesses, \
        "every read was refused, so none of them should have been performed"


def test_vec_03_an_unreadable_fault_vector_loses_the_original_cause(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(FAULT_VECTOR, FAULT_VECTOR + 1)
    states: list = directed.run(directed_image((ILLEGAL_WORD,)), memmap)

    assert len(states) == 2, "the illegal instruction is the only step the run can take"
    assert states[-1].halted, "a failed fault vector read is terminal"
    assert states[-1].csrs["fault"] == FAULT_VECTOR, \
        "FAULT describes the vector read, not the instruction which led to it"
    assert states[-1].masked_csr("cause") == CAUSE_UNMAPPED_READ, (
        "the cause of the original fault is overwritten by the cause of the vector read and is "
        "unrecoverable"
    )


@mark.xfail(reason = "misa-hw does not clear PRIVS.R on the halting vector-read path; the write "
                     "lives in TRAP_ENTER, which TRAP_HALT_CAUSE never reaches (plan section "
                     "6.10). Remove this expectation when the design writes it.",
            strict = True)
def test_vec_03_an_unreadable_fault_vector_still_leaves_machine_mode(
        directed: Directed) -> None:
    memmap: MemMap = memmap_hole(FAULT_VECTOR, FAULT_VECTOR + 1)
    states: list = directed.run(directed_image(USER_MODE_WORDS + (ILLEGAL_WORD,)), memmap)

    assert states[-1].halted
    assert states[-1].masked_csr("privs") == 0x0000, (
        "the processor sets PRIVS.R to 0 before it reads the vector, so a fault taken in User "
        "Mode must halt in Machine Mode"
    )


def test_vec_04_an_unreadable_syscall_vector_discards_the_entry(directed: Directed) -> None:
    memmap: MemMap = memmap_hole(SYSCALL_VECTOR, SYSCALL_VECTOR + 1)
    words: tuple = (set_inst(GpReg.RA, 0x42), syscall_inst(GpReg.RA), halt_inst())
    states: list = directed.run(directed_image(words), memmap)
    entry: MisaState = expect_fault(states, FaultKind.UNMAPPED_ADDRESS, AccessKind.READ,
                                    at = SYSCALL_VECTOR)

    assert entry.csrs["retsc"] == 0x0000, \
        "the entry is discarded along with the rest of the event that raised the fault"
    assert not entry.in_syscall, "the discarded entry must not leave a system call context"


@mark.parametrize("low, high, kind, name", VECTOR_READS, ids = label)
def test_vec_06_a_vector_reports_the_fault_of_its_lower_byte(directed: Directed, low: str,
                                                             high: str, kind: int,
                                                             name: str) -> None:
    memmap: MemMap = memmap_with((
        (RESET_VECTOR, RESET_VECTOR, low), (TOP_WORD, TOP_WORD, high)
    ))
    states: list = directed.run(directed_image((halt_inst(),)), memmap)

    assert states[0].pc == HANDLER_BASE
    expect_fault(states, kind, AccessKind.READ, at = RESET_VECTOR)


def test_vec_07_a_vector_read_ignores_the_mpu(directed: Directed) -> None:
    states: list = directed.run(directed_image(USER_MODE_WORDS + (ILLEGAL_WORD,)))
    entry: MisaState = expect_fault(states, FaultKind.ILLEGAL,
                                    at = addr_of(len(USER_MODE_WORDS)))

    assert not entry.halted, "the vector read must succeed; vector reads are Machine Mode"
    assert entry.masked_csr("privs") == 0x0000, "the fault handler is entered in Machine Mode"


def test_vec_08_each_handler_entry_reads_only_its_own_vector(directed: Directed) -> None:
    image: bytes = directed_image(
        (syscall_inst(GpReg.R0),), blocks = {SYSCALL_BASE: (ILLEGAL_WORD,)}
    )
    states: list = directed.run(image)
    expect_fault(states, FaultKind.ILLEGAL, at = SYSCALL_BASE)

    touched: set = {
        access.addr for access in directed.journal(image).bfm.accesses
        if (access.addr >= VECTOR_TABLE_LO)
    }
    expected: set = {
        addr for vector in (RESET_VECTOR, SYSCALL_VECTOR, FAULT_VECTOR)
        for addr in (vector, vector + 1)
    }
    assert touched == expected, (
        f"the run should have read the reset, syscall and fault vectors and nothing else of the "
        f"table, but it touched {sorted(hex(addr) for addr in touched)}"
    )


MPU_PROGRAM_WORDS: tuple = csr_write_words(CsrReg.MPUCF, USER_PAGE_REGION)

CSR_READ_BACK: tuple = (
    (CsrReg.SADDR, 0xFFFF, (), "saddr"),
    (CsrReg.RADDR, 0xFFFF, (), "raddr"),
    (CsrReg.FLAGS, 0x000F, (), "flags"),
    (CsrReg.CAUSE, 0xFF3F, (), "cause"),
    (CsrReg.EXTNS, 0x000F, (), "extns"),
    (CsrReg.FAULT, 0xFFFF, (), "fault"),
    (CsrReg.RETSC, 0xFFFF, (), "retsc"),
    (CsrReg.RETIR, 0xFFFF, (), "retir"),
    (CsrReg.PRIVS, 0x0001, MPU_PROGRAM_WORDS, "privs"),
    (CsrReg.MPUCF, 0xFFFF, (), "mpucf")
)


@mark.parametrize("csr, readable, prologue, name", CSR_READ_BACK, ids = label)
def test_csr_01_reserved_register_bits_read_back_as_zero(directed: Directed, csr: int,
                                                         readable: int, prologue: tuple,
                                                         name: str) -> None:
    words: tuple = prologue + csr_write_words(csr, 0xFFFF) + (
        rsr_inst(csr, GpReg.RA, GpReg.RB),
    )
    states: list = directed.run(directed_image(words), steps = len(words))
    final: MisaState = states[-1]
    produced: int = (final.gp_regs[GpReg.RA] << 8) | final.gp_regs[GpReg.RB]

    assert final.pc == addr_of(len(words)), "the write and the read back both had to succeed"
    assert produced == readable, (
        f"{CsrReg(csr).name} was written 0xffff and read back {produced:#06x} where the mask "
        f"table of the plan's section 3.1 gives {readable:#06x}"
    )


def test_csr_02_a_read_into_one_register_twice_keeps_the_low_byte(directed: Directed) -> None:
    words: tuple = csr_write_words(CsrReg.SADDR, 0x1234) + (
        rsr_inst(CsrReg.SADDR, GpReg.RA, GpReg.RA), halt_inst()
    )
    final: MisaState = expect_no_fault(directed.run(directed_image(words)))

    assert final.gp_regs[GpReg.RA] == 0x34, (
        "RSR writes the high byte and then the low byte, so naming the same destination twice "
        "leaves the low byte"
    )


def test_csr_03_a_write_from_r0_writes_zero(directed: Directed) -> None:
    words: tuple = csr_write_words(CsrReg.SADDR, 0x1234) + (
        wsr_inst(CsrReg.SADDR, GpReg.R0, GpReg.R0), halt_inst()
    )
    final: MisaState = expect_no_fault(directed.run(directed_image(words)))

    assert final.csrs["saddr"] == 0x0000, "a WSR from R0 writes zero rather than nothing"


def test_csr_04_cause_is_retained_until_something_overwrites_it(directed: Directed) -> None:
    handler: tuple = (
        set_inst(GpReg.RA, 0x01), wsr_inst(CsrReg.CAUSE, GpReg.R0, GpReg.R0), halt_inst()
    )
    image: bytes = directed_image((ILLEGAL_WORD,), blocks = {HANDLER_BASE: handler})
    states: list = directed.run(image)
    index: int = handler_index(states)

    assert states[index].masked_csr("cause") == cause_word(FaultKind.ILLEGAL)
    assert states[index + 1].masked_csr("cause") == cause_word(FaultKind.ILLEGAL), \
        "an unrelated instruction does not disturb CAUSE"
    assert states[index + 2].masked_csr("cause") == 0x0000, \
        "software can clear CAUSE with a WSR once it has handled it"
    assert all(
        cause_reason(state.masked_csr("cause")) != 0b000 for state in states[index:index + 2]
    ), "the processor never returns CAUSE to the Reason None state itself"
    assert states[-1].masked_csr("cause") == HALT_CAUSE, "the HALT records its own cause"


def test_csr_05_a_successful_halt_or_syscall_does_not_update_fault(directed: Directed) -> None:
    image: bytes = directed_image(
        (ILLEGAL_WORD,),
        blocks = {HANDLER_BASE: (syscall_inst(GpReg.R0),), SYSCALL_BASE: (halt_inst(),)}
    )
    states: list = directed.run(image)
    index: int = handler_index(states)

    assert states[index].csrs["fault"] == PROGRAM_BASE, "the illegal instruction set FAULT"
    assert states[-1].halted and states[-1].in_syscall, "the syscall and the HALT both ran"
    assert all(state.csrs["fault"] == PROGRAM_BASE for state in states[index:]), (
        "successful execution of HALT and SYSCALL must not update FAULT, so it still names the "
        "instruction which faulted"
    )


STALE_LINK: int = 0xBEEF


def test_csr_06_a_jal_links_only_when_it_is_taken(directed: Directed) -> None:
    words: tuple = csr_write_words(CsrReg.RADDR, STALE_LINK) + (
        set_inst(GpReg.RA, SECOND_BASE >> 8),
        set_inst(GpReg.RB, SECOND_BASE & 0xFF),
        jal_inst(CmpFlag.EQUAL, GpReg.RA, GpReg.RB),
        jal_inst(CmpFlag.ALWAYS, GpReg.RA, GpReg.RB)
    )
    states: list = directed.run(directed_image(words))
    final: MisaState = expect_no_fault(states)

    assert state_at(states, addr_of(6)).csrs["raddr"] == STALE_LINK, (
        "FLAGS is zero, so the EQUAL branch was not taken; a JAL which is not taken computes no "
        "link address and leaves RADDR alone"
    )
    assert final.pc == SECOND_BASE and final.halted, "the ALWAYS branch was taken"
    assert final.csrs["raddr"] == addr_of(7), \
        "a taken JAL links the address of the next instruction"


def test_csr_07_a_link_which_would_leave_the_address_range_overflows(
        directed: Directed) -> None:
    taken: int = jal_inst(CmpFlag.ALWAYS, GpReg.RA, GpReg.RB)
    image: bytes = directed_image((halt_inst(),), vectors = {RESET_VECTOR: taken})
    state: MisaState = directed.seed(image, pc = RESET_VECTOR)

    states: list = directed.resume(state, image, steps = 1)
    entry: MisaState = expect_fault(states, FaultKind.ADDR_OVERFLOW, OverflowKind.LINK,
                                    at = RESET_VECTOR)

    assert entry.csrs["raddr"] == 0x0000, "the overflowing link must not be written"


SYSCALL_CODE: int = 0x42


def context_status(in_syscall: bool, in_interrupt: bool) -> int:
    return int(in_syscall) | (int(in_interrupt) << 1)


def return_status(csr: int, in_syscall: bool = False, in_interrupt: bool = False) -> int:
    return context_status(in_syscall, in_interrupt) | (csr << 4)


def test_sc_01_a_syscall_enters_its_handler_with_the_documented_state(
        directed: Directed) -> None:
    words: tuple = USER_MODE_WORDS + (
        set_inst(GpReg.RA, SYSCALL_CODE), syscall_inst(GpReg.RA)
    )
    states: list = directed.run(directed_image(words))
    entry: MisaState = state_at(states, SYSCALL_BASE)

    assert entry.masked_csr("cause") == cause_word(FaultKind.HALT, SYSCALL_CODE), \
        "CAUSE is the RS operand in the extended status over the Halt instruction type"
    assert entry.csrs["retsc"] == addr_of(len(words)), "RETSC is the address after the SYSCALL"
    assert entry.masked_csr("privs") == 0x0000, "the handler is entered in Machine Mode"
    assert entry.syscall_privs == USER_MODE, "the ring bit the call was made from is saved"
    assert entry.in_syscall and not entry.in_interrupt
    assert entry.csrs["fault"] == 0x0000, "a successful SYSCALL does not touch FAULT"
    assert entry.gp_regs[GpReg.RA] == SYSCALL_CODE, "the general purpose registers are untouched"


def test_sc_02_a_syscall_without_the_exception_bit_is_a_halt(directed: Directed) -> None:
    words: tuple = (set_inst(GpReg.RA, SYSCALL_CODE), halt_inst(GpReg.RA))
    states: list = directed.run(directed_image(words))
    final: MisaState = states[-1]

    assert final.halted, "HALT is terminal"
    assert final.pc == addr_of(1), "a halted processor does not advance past the HALT"
    assert final.masked_csr("cause") == cause_word(FaultKind.HALT, SYSCALL_CODE)
    assert not final.in_syscall, "no handler was entered"


def test_sc_03_a_syscall_inside_a_system_call_context_is_improper(directed: Directed) -> None:
    image: bytes = directed_image(
        (syscall_inst(GpReg.R0),), blocks = {SYSCALL_BASE: (syscall_inst(GpReg.R0),)}
    )
    states: list = directed.run(image)
    entry: MisaState = expect_fault(states, FaultKind.IMPROPER_SYSCALL,
                                    context_status(True, False), at = SYSCALL_BASE)

    assert entry.in_syscall, "the fault handler runs in the context the fault was detected in"


def test_sc_04_a_syscall_inside_an_interrupt_context_is_improper(directed: Directed) -> None:
    image: bytes = directed_image((syscall_inst(GpReg.R0),))
    state: MisaState = directed.seed(image, in_interrupt = True)

    states: list = directed.resume(state, image, steps = 1)
    expect_fault(states, FaultKind.IMPROPER_SYSCALL, context_status(False, True))


def test_sc_05_an_improper_syscall_outranks_the_entry_faults(directed: Directed) -> None:
    image: bytes = directed_image((halt_inst(),),
                                  vectors = {RESET_VECTOR: syscall_inst(GpReg.R0)})
    memmap: MemMap = memmap_hole(SYSCALL_VECTOR, SYSCALL_VECTOR + 1)
    state: MisaState = directed.seed(image, memmap, pc = RESET_VECTOR, in_syscall = True)

    states: list = directed.resume(state, image, memmap, steps = 1)
    expect_fault(states, FaultKind.IMPROPER_SYSCALL, context_status(True, False),
                 at = RESET_VECTOR)


def test_sc_06_a_syscall_whose_link_would_overflow_faults(directed: Directed) -> None:
    image: bytes = directed_image((halt_inst(),),
                                  vectors = {RESET_VECTOR: syscall_inst(GpReg.R0)})
    state: MisaState = directed.seed(image, pc = RESET_VECTOR)

    states: list = directed.resume(state, image, steps = 1)
    entry: MisaState = expect_fault(states, FaultKind.ADDR_OVERFLOW, OverflowKind.LINK,
                                    at = RESET_VECTOR)

    assert entry.csrs["retsc"] == 0x0000 and not entry.in_syscall, \
        "the discarded entry leaves RETSC and the context alone"


def test_sc_07_a_return_outside_a_system_call_context_is_improper(directed: Directed) -> None:
    states: list = directed.run(directed_image((jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),)))

    expect_fault(states, FaultKind.IMPROPER_RETURN, return_status(CsrReg.RETSC))


def test_sc_08_a_return_from_a_syscall_inside_an_interrupt_is_improper(
        directed: Directed) -> None:
    image: bytes = directed_image((jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),))
    state: MisaState = directed.seed(image, in_syscall = True, in_interrupt = True)

    states: list = directed.resume(state, image, steps = 1)
    expect_fault(states, FaultKind.IMPROPER_RETURN,
                 return_status(CsrReg.RETSC, in_syscall = True, in_interrupt = True))


CONDITIONAL_RETURNS: tuple = (
    (0x0, CmpFlag.EQUAL, "false"), (flags_word(1, 0, 0, 0), CmpFlag.EQUAL, "true"),
    (0x0, CmpFlag.NOT_EQUAL, "true-not-equal")
)


@mark.parametrize("flags_in, cmp_flag, name", CONDITIONAL_RETURNS, ids = label)
def test_sc_09_a_conditional_context_return_faults_on_its_encoding(directed: Directed,
                                                                   flags_in: int, cmp_flag: int,
                                                                   name: str) -> None:
    words: tuple = csr_write_words(CsrReg.FLAGS, flags_in) + (
        jmp_csr_inst(cmp_flag, CsrReg.RETSC),
    )
    states: list = directed.run(directed_image(words))

    expect_fault(states, FaultKind.COND_RETURN, cmp_flag | (CsrReg.RETSC << 4),
                 at = addr_of(len(words) - 1))


def test_sc_09_a_conditional_context_return_faults_inside_the_context_too(
        directed: Directed) -> None:
    image: bytes = directed_image(
        (syscall_inst(GpReg.R0),),
        blocks = {SYSCALL_BASE: (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),)}
    )
    conditional: bytes = directed_image(
        (syscall_inst(GpReg.R0),),
        blocks = {SYSCALL_BASE: (jmp_csr_inst(CmpFlag.NOT_EQUAL, CsrReg.RETSC),)}
    )

    assert not state_at(directed.run(image), addr_of(1)).in_syscall, \
        "the ALWAYS form of the same return succeeds and leaves the context"

    states: list = directed.run(conditional)
    expect_fault(states, FaultKind.COND_RETURN, CmpFlag.NOT_EQUAL | (CsrReg.RETSC << 4),
                 at = SYSCALL_BASE)


def test_sc_10_a_conditional_return_outranks_an_improper_context(directed: Directed) -> None:
    states: list = directed.run(directed_image((jmp_csr_inst(CmpFlag.EQUAL, CsrReg.RETSC),)))
    entry: MisaState = expect_fault(states, FaultKind.COND_RETURN,
                                    CmpFlag.EQUAL | (CsrReg.RETSC << 4))

    assert entry.masked_csr("cause") != cause_word(FaultKind.IMPROPER_RETURN,
                                                   return_status(CsrReg.RETSC))


def test_sc_11_a_return_from_a_syscall_restores_the_saved_ring_bit(directed: Directed) -> None:
    words: tuple = USER_MODE_WORDS + (syscall_inst(GpReg.R0),)
    image: bytes = directed_image(
        words, blocks = {SYSCALL_BASE: (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),)}
    )
    states: list = directed.run(image)
    returned: MisaState = state_at(states, addr_of(len(words)))

    assert returned.masked_csr("privs") == USER_MODE, \
        "the ring bit is restored to its value before the SYSCALL"
    assert not returned.in_syscall, "and the context is left"
    assert returned.syscall_privs == USER_MODE, "the saved slot itself is not cleared"


SAVED_RSCRATCH: int = 0x1234
LIVE_RSCRATCH: tuple = (0xAA, 0xBB)


def test_int_02_a_return_restores_the_automatically_saved_registers(
        directed: Directed) -> None:
    image: bytes = directed_image(
        (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETIR),), blocks = {SECOND_BASE: (halt_inst(),)}
    )
    state: MisaState = directed.seed(
        image, in_interrupt = True, int_rscratch = SAVED_RSCRATCH, int_flags = ALL_FLAGS,
        csrs = {"retir": SECOND_BASE, "flags": 0x0000},
        regs = {GpReg.RSCRATCH0: LIVE_RSCRATCH[0], GpReg.RSCRATCH1: LIVE_RSCRATCH[1]}
    )

    states: list = directed.resume(state, image)
    returned: MisaState = state_at(states, SECOND_BASE)

    assert returned.gp_regs[GpReg.RSCRATCH0] == SAVED_RSCRATCH >> 8
    assert returned.gp_regs[GpReg.RSCRATCH1] == SAVED_RSCRATCH & 0xFF
    assert returned.masked_csr("flags") == ALL_FLAGS, \
        "RSCRATCH0, RSCRATCH1 and FLAGS are restored from storage software cannot address"
    assert not returned.in_interrupt, "and the interrupt context is left"
    assert states[-1].halted


def test_int_03_a_return_restores_the_ring_bit_from_its_own_slot(directed: Directed) -> None:
    image: bytes = directed_image(
        (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETIR),), blocks = {SECOND_BASE: (halt_inst(),)}
    )
    state: MisaState = directed.seed(
        image, in_interrupt = True, int_privs = USER_MODE, syscall_privs = 0x0000,
        csrs = {"retir": SECOND_BASE, "privs": 0x0000}
    )

    states: list = directed.resume(state, image)
    returned: MisaState = state_at(states, SECOND_BASE)

    assert returned.masked_csr("privs") == USER_MODE, (
        "the interrupt return restores the ring bit from the interrupt slot, not from the system "
        "call slot, which holds Machine Mode here"
    )
    expect_fault(states, FaultKind.PRIV_ACCESS, AccessKind.EXECUTE, at = SECOND_BASE)


def test_int_04_the_two_context_returns_restore_independent_ring_bits(
        directed: Directed) -> None:
    image: bytes = directed_image(
        (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETIR),),
        blocks = {
            SECOND_BASE: (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),),
            THIRD_BASE: (halt_inst(),)
        }
    )
    state: MisaState = directed.seed(
        image, in_syscall = True, in_interrupt = True, int_privs = 0x0000,
        syscall_privs = USER_MODE,
        csrs = {"retir": SECOND_BASE, "retsc": THIRD_BASE, "privs": 0x0000}
    )

    states: list = directed.resume(state, image)
    from_interrupt: MisaState = state_at(states, SECOND_BASE)
    from_syscall: MisaState = state_at(states, THIRD_BASE)

    assert from_interrupt.masked_csr("privs") == 0x0000, "the interrupt was taken in Machine Mode"
    assert not from_interrupt.in_interrupt and from_interrupt.in_syscall, \
        "leaving the interrupt context leaves the system call context it nested inside"
    assert from_syscall.masked_csr("privs") == USER_MODE, \
        "the system call return restores the ring bit the call was made from"
    assert not from_syscall.in_syscall


def test_int_05_a_return_outside_an_interrupt_context_is_improper(directed: Directed) -> None:
    states: list = directed.run(directed_image((jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETIR),)))

    expect_fault(states, FaultKind.IMPROPER_RETURN, return_status(CsrReg.RETIR))


@mark.parametrize("flags_in, cmp_flag, name", CONDITIONAL_RETURNS, ids = label)
def test_int_06_a_conditional_interrupt_return_faults_on_its_encoding(directed: Directed,
                                                                      flags_in: int,
                                                                      cmp_flag: int,
                                                                      name: str) -> None:
    words: tuple = csr_write_words(CsrReg.FLAGS, flags_in) + (
        jmp_csr_inst(cmp_flag, CsrReg.RETIR),
    )
    states: list = directed.run(directed_image(words))

    expect_fault(states, FaultKind.COND_RETURN, cmp_flag | (CsrReg.RETIR << 4),
                 at = addr_of(len(words) - 1))


DATA_PAGE: int = DATA_BASE >> 8
PROBE_PAGE: int = REFUSED_PAGE >> 8

USER_CSR_WORDS: dict = {
    "rsr": lambda csr: rsr_inst(csr, GpReg.RA, GpReg.RB),
    "wsr": lambda csr: wsr_inst(csr, GpReg.RA, GpReg.RB),
    "jmp": lambda csr: jmp_csr_inst(CmpFlag.ALWAYS, csr)
}
USER_ALLOWED: dict = {
    "rsr": USER_READABLE_CSRS, "wsr": USER_WRITABLE_CSRS, "jmp": USER_READABLE_CSRS
}
USER_JMP_TARGETS: dict = {
    CsrReg.SADDR: 0x0000, CsrReg.RADDR: 0x0000, CsrReg.FLAGS: 0x0000, CsrReg.PRIVS: USER_MODE
}


def load_words(addr: int, rd: int = GpReg.RC) -> tuple:
    return (
        set_inst(GpReg.RA, addr >> 8),
        set_inst(GpReg.RB, addr & 0xFF),
        load_inst(rd, GpReg.RA, 0)
    )


def user_mode_words(regions: tuple) -> tuple:
    words: tuple = ()
    for region in regions:
        words += csr_write_words(CsrReg.MPUCF, region)
    return words + csr_write_words(CsrReg.PRIVS, USER_MODE)


def test_prv_01_halt_in_user_mode_is_a_privileged_instruction(directed: Directed) -> None:
    states: list = directed.run(directed_image(USER_MODE_WORDS + (halt_inst(),)))
    entry: MisaState = expect_fault(states, FaultKind.PRIV_INSTRUCTION,
                                    at = addr_of(len(USER_MODE_WORDS)))

    assert not entry.halted, "the HALT is preempted, not performed"


@mark.parametrize("csr", ALL_CSRS, ids = lambda csr: CsrReg(csr).name.lower())
@mark.parametrize("named", ["rsr", "wsr", "jmp"])
def test_prv_02_user_mode_reaches_only_the_registers_the_manual_lists(directed: Directed,
                                                                      named: str,
                                                                      csr: int) -> None:
    words: tuple = USER_MODE_WORDS + (USER_CSR_WORDS[named](csr),)
    states: list = directed.run(directed_image(words), steps = len(words))
    final: MisaState = states[-1]

    if (csr not in USER_ALLOWED[named]):
        expect_fault(states, FaultKind.PRIV_INSTRUCTION, at = addr_of(len(words) - 1))
        return

    expected: int = USER_JMP_TARGETS[csr] if (named == "jmp") else addr_of(len(words))
    assert final.pc == expected, (
        f"{named.upper()} on {CsrReg(csr).name} is allowed from User Mode, so the instruction "
        f"should have completed and left pc at {expected:#06x}"
    )
    assert final.masked_csr("cause") == 0x0000, "no fault was raised"


def test_prv_03_user_mode_cannot_write_the_registers_it_can_read(directed: Directed) -> None:
    assert CsrReg.PRIVS in USER_READABLE_CSRS and CsrReg.PRIVS not in USER_WRITABLE_CSRS, \
        "PRIVS is readable but not writable from User Mode"

    readable: tuple = USER_MODE_WORDS + (rsr_inst(CsrReg.PRIVS, GpReg.RA, GpReg.RB),)
    states: list = directed.run(directed_image(readable), steps = len(readable))
    assert states[-1].gp_regs[GpReg.RB] == USER_MODE, "the ring bit reads back as User Mode"

    writable: tuple = USER_MODE_WORDS + (wsr_inst(CsrReg.PRIVS, GpReg.R0, GpReg.R0),)
    expect_fault(directed.run(directed_image(writable), steps = len(writable)),
                 FaultKind.PRIV_INSTRUCTION, at = addr_of(len(writable) - 1))


def test_prv_04_a_jump_through_a_special_register_follows_the_readable_set(
        directed: Directed) -> None:
    allowed: tuple = USER_MODE_WORDS + (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.SADDR),)
    states: list = directed.run(directed_image(allowed), steps = len(allowed))
    assert states[-1].pc == 0x0000, "SADDR is readable from User Mode and holds zero"

    denied: tuple = USER_MODE_WORDS + (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.CAUSE),)
    expect_fault(directed.run(directed_image(denied), steps = len(denied)),
                 FaultKind.PRIV_INSTRUCTION, at = addr_of(len(denied) - 1))


CONTEXT_RETURNS: tuple = (
    (CsrReg.RETSC, CmpFlag.ALWAYS, "retsc-always"),
    (CsrReg.RETSC, CmpFlag.EQUAL, "retsc-equal"),
    (CsrReg.RETIR, CmpFlag.ALWAYS, "retir-always"),
    (CsrReg.RETIR, CmpFlag.EQUAL, "retir-equal")
)


@mark.parametrize("csr, cmp_flag, name", CONTEXT_RETURNS, ids = label)
def test_prv_05_a_privileged_instruction_outranks_both_control_event_faults(
        directed: Directed, csr: int, cmp_flag: int, name: str) -> None:
    words: tuple = USER_MODE_WORDS + (jmp_csr_inst(cmp_flag, csr),)
    states: list = directed.run(directed_image(words))
    entry: MisaState = expect_fault(states, FaultKind.PRIV_INSTRUCTION,
                                    at = addr_of(len(USER_MODE_WORDS)))

    assert entry.masked_csr("cause") not in (
        cause_word(FaultKind.COND_RETURN, cmp_flag | (csr << 4)),
        cause_word(FaultKind.IMPROPER_RETURN, return_status(csr))
    ), "User Mode has no legitimate use for either return, so neither fault is the one reported"


def test_prv_06_the_mpu_grants_nothing_after_reset(directed: Directed) -> None:
    words: tuple = csr_write_words(CsrReg.PRIVS, USER_MODE) + (halt_inst(),)
    states: list = directed.run(directed_image(words))

    assert states[0].mpu_regions == (0x0000,) * 4, "all four regions reset to zero"
    assert states[0].mpu_index == 0, "and the window names region zero"
    expect_fault(states, FaultKind.PRIV_ACCESS, AccessKind.EXECUTE, at = addr_of(3))


def test_prv_07_machine_mode_ignores_the_mpu(directed: Directed) -> None:
    denying: int = mpu_region(PROGRAM_PAGE, 0, 0, 0b000)
    words: tuple = csr_write_words(CsrReg.MPUCF, denying) + load_words(DATA_BASE) + (
        store_inst(GpReg.RC, GpReg.RA, 1), halt_inst()
    )
    image: bytes = directed_image(words, data = MEM_DATA)
    final: MisaState = expect_no_fault(directed.run(image))

    assert final.mpu_regions[0] == denying, "the region which grants nothing was configured"
    assert final.gp_regs[GpReg.RC] == MEM_PATTERN[0], "the fetch and the load were unaffected"
    assert final.memory[DATA_BASE + 1] == MEM_PATTERN[0], "and so was the store"


SIZED_REGION: int = mpu_region(0x05, 1, 1, CAPS_READ)
SIZED_PAGES: tuple = tuple(mpu_pages(0x05, 1))
SIZED_FIRST: int = SIZED_PAGES[0] << 8
SIZED_LAST: int = (SIZED_PAGES[-1] << 8) | 0xFF
SIZED_PROBES: tuple = (
    (SIZED_FIRST - 1, False, "below"), (SIZED_FIRST, True, "first"),
    (SIZED_LAST, True, "last"), (SIZED_LAST + 1, False, "above")
)
PROBE_DATUM: int = 0xC3


@mark.parametrize("addr, allowed, name", SIZED_PROBES, ids = label)
def test_prv_08_a_region_covers_two_to_the_size_pages_from_its_aligned_base(
        directed: Directed, addr: int, allowed: bool, name: str) -> None:
    words: tuple = user_mode_words((USER_PAGE_REGION, SIZED_REGION)) + load_words(addr)
    image: bytes = directed_image(words, data = {addr: bytes((PROBE_DATUM,))})
    states: list = directed.run(image, steps = len(words))

    if (not allowed):
        expect_fault(states, FaultKind.PRIV_ACCESS, AccessKind.READ,
                     at = addr_of(len(words) - 1))
        return

    assert states[-1].gp_regs[GpReg.RC] == PROBE_DATUM, (
        f"{addr:#06x} is inside the region, which spans pages "
        f"{SIZED_PAGES[0]:#04x} to {SIZED_PAGES[-1]:#04x} from the aligned base"
    )


OVERLAPPING: tuple = (
    ((CAPS_READ, 0b000), True, "low-index-grants"),
    ((0b000, CAPS_READ), False, "low-index-denies")
)


@mark.parametrize("reversed_order", [False, True], ids = ["in-order", "reversed"])
@mark.parametrize("caps, allowed, name", OVERLAPPING, ids = label)
def test_prv_09_the_lowest_index_decides_an_overlap(directed: Directed, caps: tuple,
                                                    allowed: bool, name: str,
                                                    reversed_order: bool) -> None:
    regions: tuple = (
        mpu_region(PROBE_PAGE, 0, 0, caps[0]), mpu_region(PROBE_PAGE, 0, 1, caps[1])
    )
    if (reversed_order):
        regions = tuple(reversed(regions))

    words: tuple = user_mode_words(
        (mpu_region(PROGRAM_PAGE, 0, 2, CAPS_RWX),) + regions
    ) + load_words(REFUSED_PAGE)
    image: bytes = directed_image(words, data = {REFUSED_PAGE: bytes((PROBE_DATUM,))})
    states: list = directed.run(image, steps = len(words))

    if (not allowed):
        expect_fault(states, FaultKind.PRIV_ACCESS, AccessKind.READ,
                     at = addr_of(len(words) - 1))
        return

    assert states[-1].gp_regs[GpReg.RC] == PROBE_DATUM, \
        "region 0 grants the read and its permissions are the ones which apply"


def test_prv_10_the_ignored_base_bits_are_retained_and_read_back(directed: Directed) -> None:
    words: tuple = csr_write_words(CsrReg.MPUCF, SIZED_REGION) + (
        rsr_inst(CsrReg.MPUCF, GpReg.RA, GpReg.RB),
    )
    states: list = directed.run(directed_image(words), steps = len(words))
    final: MisaState = states[-1]
    produced: int = (final.gp_regs[GpReg.RA] << 8) | final.gp_regs[GpReg.RB]

    assert (SIZED_REGION >> 8) != SIZED_PAGES[0], "the case needs a base which is not aligned"
    assert produced == SIZED_REGION, (
        f"MPUCF read back {produced:#06x} where the write was {SIZED_REGION:#06x}; the ignored "
        f"low base bits are retained rather than illegal"
    )


@mark.parametrize("caps, allowed, name",
                  [(CAPS_READ, True, "granted"), (0b000, False, "denied")], ids = label)
def test_prv_11_a_region_write_governs_the_instructions_after_it(directed: Directed, caps: int,
                                                                 allowed: bool,
                                                                 name: str) -> None:
    region: int = mpu_region(DATA_PAGE, 0, 1, caps)
    configured: tuple = user_mode_words((USER_PAGE_REGION, region))
    words: tuple = configured + load_words(DATA_BASE)
    image: bytes = directed_image(words, data = MEM_DATA)
    states: list = directed.run(image, steps = len(words))

    settled: MisaState = state_at(states, addr_of(6))
    assert settled.mpu_regions[1] == region, \
        "the region register is written on the step the WSR takes, not a later one"
    assert settled.masked_csr("privs") == 0x0000, \
        "and the WSR which follows it still runs in Machine Mode"

    if (not allowed):
        expect_fault(states, FaultKind.PRIV_ACCESS, AccessKind.READ,
                     at = addr_of(len(words) - 1))
        return
    assert states[-1].gp_regs[GpReg.RC] == MEM_PATTERN[0], \
        "the load which follows the write is governed by the region it wrote"


DENIED_ACCESSES: tuple = (
    (CAPS_READ | CAPS_WRITE, (), AccessKind.EXECUTE, "fetch"),
    (CAPS_RWX, load_words(DATA_BASE), AccessKind.READ, "load"),
    (CAPS_RWX, load_words(DATA_BASE)[:2] + (store_inst(GpReg.RC, GpReg.RA, 0),),
     AccessKind.WRITE, "store")
)


@mark.parametrize("caps, access, status, name", DENIED_ACCESSES, ids = label)
def test_prv_12_an_mpu_denial_names_the_access_it_refused(directed: Directed, caps: int,
                                                          access: tuple, status: int,
                                                          name: str) -> None:
    words: tuple = user_mode_words((mpu_region(PROGRAM_PAGE, 0, 0, caps),)) + access
    states: list = directed.run(directed_image(words))
    at: int = addr_of(len(words) - 1) if (access) else addr_of(len(words))

    expect_fault(states, FaultKind.PRIV_ACCESS, status, at = at)


def test_prv_13_a_disabled_privilege_extension_behaves_as_machine_mode(
        directed: Directed) -> None:
    image: bytes = directed_image(load_words(DATA_BASE) + (halt_inst(),), data = MEM_DATA)
    state: MisaState = directed.seed(image, extns = NO_PRIVILEGE, csrs = {"privs": USER_MODE})

    states: list = directed.resume(state, image)
    final: MisaState = expect_no_fault(states)

    assert final.halted and final.gp_regs[GpReg.RC] == MEM_PATTERN[0], \
        "the fetches and the load all behaved as though in Machine Mode"
    assert final.masked_csr("privs") == USER_MODE, \
        "disabling the extension does not clear PRIVS"


def test_prv_14_a_context_return_without_the_extension_does_not_restore_the_ring_bit(
        directed: Directed) -> None:
    image: bytes = directed_image(
        (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),), blocks = {SECOND_BASE: (halt_inst(),)}
    )
    state: MisaState = directed.seed(
        image, extns = NO_PRIVILEGE, in_syscall = True, syscall_privs = USER_MODE,
        csrs = {"privs": 0x0000, "retsc": SECOND_BASE}
    )

    states: list = directed.resume(state, image)
    returned: MisaState = state_at(states, SECOND_BASE)

    assert returned.masked_csr("privs") == 0x0000, (
        "a context return performed while the Privilege Extension is disabled must not restore "
        "the saved ring bit"
    )
    assert not returned.in_syscall and states[-1].halted, "the return itself still happened"


@mark.parametrize("saved", [0x0000, USER_MODE], ids = ["machine", "user"])
def test_prv_15_a_write_inside_a_context_does_not_survive_the_return(directed: Directed,
                                                                     saved: int) -> None:
    image: bytes = directed_image(
        csr_write_words(CsrReg.PRIVS, USER_MODE),
        blocks = {
            HANDLER_BASE: (jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC),),
            SECOND_BASE: (halt_inst(),)
        }
    )
    state: MisaState = directed.seed(
        image, in_syscall = True, syscall_privs = saved,
        csrs = {"privs": 0x0000, "retsc": SECOND_BASE}
    )

    states: list = directed.resume(state, image, steps = 5)
    assert states[3].masked_csr("privs") == USER_MODE, "the write inside the context took effect"
    expect_fault(states, FaultKind.PRIV_ACCESS, AccessKind.EXECUTE, at = addr_of(3))

    returned: MisaState = states[-1]
    assert returned.pc == SECOND_BASE, "the fault handler returned from the system call"
    assert returned.masked_csr("privs") == saved, (
        f"the return restores PRIVS entirely from the saved slot, which holds {saved:#06x}, "
        f"rather than leaving what the WSR inside the context wrote"
    )
    assert not returned.in_syscall


ONLY_DYNAMICHW: int = Extn.DYNAMICHW
GATED_READ: int = rsr_inst(CsrReg.RETSC, GpReg.RA, GpReg.RB)

EXTNS_CHANGES: tuple = (
    (DEFAULT_EXTNS, ONLY_DYNAMICHW, False, "disable"),
    (ONLY_DYNAMICHW, DEFAULT_EXTNS, True, "enable")
)


@mark.parametrize("extns, written, legal, name", EXTNS_CHANGES, ids = label)
def test_dhw_01_a_write_to_extns_takes_effect_on_the_next_instruction(directed: Directed,
                                                                      extns: int, written: int,
                                                                      legal: bool,
                                                                      name: str) -> None:
    words: tuple = csr_write_words(CsrReg.EXTNS, written) + (GATED_READ, halt_inst())
    states: list = directed.run(directed_image(words), extns = extns)
    settled: MisaState = state_at(states, addr_of(3))

    assert settled.masked_csr("extns") == written, \
        "the write lands on the step the WSR takes, so the instruction after it sees it"
    assert settled.masked_csr("cause") == 0x0000, "the WSR itself cannot fault on its own write"

    if (not legal):
        expect_fault(states, FaultKind.ILLEGAL, at = addr_of(3))
        return
    assert expect_no_fault(states).halted, \
        "the same instruction decodes once the extension it names is enabled"


IGNORED_BIT_WRITES: tuple = (
    (DEFAULT_EXTNS, DEFAULT_EXTNS & ~Extn.DYNAMICHW, DEFAULT_EXTNS, "clear-while-set"),
    (ONLY_DYNAMICHW, 0x0, ONLY_DYNAMICHW, "clear-alone")
)


@mark.parametrize("extns, written, readable, name", IGNORED_BIT_WRITES, ids = label)
def test_dhw_02_the_dynamic_hardware_bit_ignores_writes(directed: Directed, extns: int,
                                                        written: int, readable: int,
                                                        name: str) -> None:
    words: tuple = csr_write_words(CsrReg.EXTNS, written) + (
        rsr_inst(CsrReg.EXTNS, GpReg.RA, GpReg.RB),
    )
    states: list = directed.run(directed_image(words), steps = len(words), extns = extns)
    produced: int = states[-1].gp_regs[GpReg.RB]

    assert produced == readable, (
        f"writing {written:#x} over {extns:#x} left EXTNS={produced:#x}; bit 0 must ignore "
        f"writes so that the extension cannot disable itself"
    )


@mark.parametrize("written", [DEFAULT_EXTNS, 0x0], ids = hex)
def test_dhw_03_a_write_without_the_extension_is_ignored_entirely(directed: Directed,
                                                                  written: int) -> None:
    words: tuple = csr_write_words(CsrReg.EXTNS, written) + (
        rsr_inst(CsrReg.EXTNS, GpReg.RA, GpReg.RB),
    )
    states: list = directed.run(directed_image(words), steps = len(words),
                                extns = NO_DYNAMICHW)

    assert states[-1].gp_regs[GpReg.RB] == NO_DYNAMICHW, (
        "without the Dynamic Hardware Extension the list of supported extensions is a property "
        "of the hardware and every write to EXTNS is ignored"
    )


PRESERVED_RETSC: int = 0x1234
PRESERVED_RETIR: int = 0x5678


def preserving_words(tail: tuple) -> tuple:
    return (
        csr_write_words(CsrReg.RETSC, PRESERVED_RETSC)
        + csr_write_words(CsrReg.RETIR, PRESERVED_RETIR)
        + csr_write_words(CsrReg.MPUCF, SIZED_REGION)
        + csr_write_words(CsrReg.EXTNS, ONLY_DYNAMICHW)
        + (set_inst(GpReg.RA, 0x11),)
        + tail
    )


READ_BACK_WORDS: tuple = csr_write_words(CsrReg.EXTNS, DEFAULT_EXTNS) + (
    rsr_inst(CsrReg.RETSC, GpReg.RB, GpReg.RC),
    rsr_inst(CsrReg.RETIR, GpReg.RD, GpReg.RE),
    rsr_inst(CsrReg.MPUCF, GpReg.RF, GpReg.RU),
    halt_inst()
)


def test_dhw_04_disabling_an_extension_preserves_its_registers_and_contexts(
        directed: Directed) -> None:
    image: bytes = directed_image(preserving_words(READ_BACK_WORDS))
    state: MisaState = directed.seed(image, in_syscall = True, in_interrupt = True)

    states: list = directed.resume(state, image)
    final: MisaState = expect_no_fault(states)

    assert final.halted
    assert (final.gp_regs[GpReg.RB] << 8) | final.gp_regs[GpReg.RC] == PRESERVED_RETSC
    assert (final.gp_regs[GpReg.RD] << 8) | final.gp_regs[GpReg.RE] == PRESERVED_RETIR
    assert (final.gp_regs[GpReg.RF] << 8) | final.gp_regs[GpReg.RU] == SIZED_REGION, \
        "the registers an extension defines keep their contents while it is disabled"
    assert final.in_syscall and final.in_interrupt, \
        "and so does whether the processor is in a system call or an interrupt context"
    assert final.gp_regs[GpReg.RA] == 0x11, "the instruction run while disabled still ran"


def test_dhw_05_re_enabling_an_extension_restores_access_to_it(directed: Directed) -> None:
    while_disabled: bytes = directed_image(preserving_words((GATED_READ,)))
    after_enabling: bytes = directed_image(preserving_words(READ_BACK_WORDS))
    attempted: int = addr_of(len(preserving_words(())))

    expect_fault(directed.run(while_disabled), FaultKind.ILLEGAL, at = attempted)

    final: MisaState = expect_no_fault(directed.run(after_enabling))
    assert (final.gp_regs[GpReg.RB] << 8) | final.gp_regs[GpReg.RC] == PRESERVED_RETSC, \
        "re-enabling the extension makes the preserved value readable again"


def test_rst_01_reset_clears_every_register_except_extns(directed: Directed) -> None:
    states: list = directed.run(directed_image((halt_inst(),)), steps = 0)
    start: MisaState = states[0]

    assert len(states) == 1, "a run of no steps is the reset state and nothing else"
    assert start.gp_regs == (0x00,) * 16, "every general purpose register resets to zero"
    assert start.masked_csr("extns") == DEFAULT_EXTNS, "EXTNS carries the supported extensions"
    for name in ("saddr", "raddr", "flags", "cause", "fault", "retsc", "retir", "privs"):
        assert start.masked_csr(name) == 0x0000, f"{name.upper()} resets to zero"
    assert not start.halted


def test_rst_02_reset_is_not_in_a_syscall_or_interrupt_context(directed: Directed) -> None:
    start: MisaState = directed.run(directed_image((halt_inst(),)), steps = 0)[0]

    assert not start.in_syscall, "the processor must not be initialised to a syscall context"
    assert not start.in_interrupt, "nor to an interrupt context"
    assert (start.int_rscratch, start.int_flags, start.syscall_privs, start.int_privs) == \
        (0x0000, 0x0000, 0x0000, 0x0000), "and the slots those contexts restore from are zero"


@mark.parametrize("extns", [DEFAULT_EXTNS, NO_PRIVILEGE], ids = hex)
def test_rst_03_the_mpu_regions_reset_even_without_the_privilege_extension(directed: Directed,
                                                                           extns: int) -> None:
    start: MisaState = directed.run(directed_image((halt_inst(),)), steps = 0, extns = extns)[0]

    assert start.mpu_regions == (0x0000,) * 4, (
        "all four regions initialise with no User Mode permissions whether or not the extension "
        "is enabled on reset"
    )
    assert start.mpu_index == 0, "MPUCF.Index initialises to zero"
    assert start.mpucf == 0x0000


def test_rst_05_the_program_counter_is_read_from_the_reset_vector_last(
        directed: Directed) -> None:
    image: bytes = directed_image(
        (halt_inst(),), vectors = {RESET_VECTOR: SECOND_BASE},
        blocks = {SECOND_BASE: (halt_inst(),)}
    )
    states: list = directed.run(image)
    reads: list = [access.addr for access in directed.journal(image).bfm.accesses]

    assert states[0].pc == SECOND_BASE, "the program counter comes from the vector, not an alias"
    assert reads[:3] == [RESET_VECTOR, RESET_VECTOR + 1, SECOND_BASE], (
        f"the reset vector is read after every other initialisation step and before anything "
        f"else, but the run began by reading {[hex(addr) for addr in reads[:3]]}"
    )
    assert states[-1].halted
