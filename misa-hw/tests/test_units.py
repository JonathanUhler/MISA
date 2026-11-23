from amaranth import Module
from amaranth.sim import Simulator

from misa_hw.alu_unit import AluUnit
from misa_hw.cmp_unit import CmpUnit
from misa_hw.csr_reg_file import CsrRegFile
from misa_hw.decoder_unit import DecoderUnit
from misa_hw.gp_reg_file import GpRegFile
from misa_hw.grammar import AluOp, CmpFlag, CsrReg, Extn, GpReg, Op
from misa_hw.init import CsrRegInit, GpRegInit, MpuInit
from misa_hw.mpu_unit import MpuUnit

from memory import CAPS_RWX
from programs import mpu_pages, mpu_region
from state import CSR_REG_NAMES, CSR_WRITE_MASKS, GP_REG_NAMES, MPU_REGION_COUNT
from target import PackedView


class CombSim:

    def __init__(self, unit: object, inputs: tuple, outputs: tuple) -> None:
        self.module: Module = Module()
        self.module.submodules.unit = unit
        self.inputs: PackedView = PackedView(inputs)
        self.outputs: PackedView = PackedView(outputs)


    def sweep(self, vectors: object, check: object) -> int:
        tested: int = 0

        async def drive(ctx: object) -> None:
            nonlocal tested
            for vector in vectors:
                ctx.set(self.inputs.value, self.inputs.pack(vector))
                check(vector, self.outputs.unpack(ctx.get(self.outputs.value)))
                tested += 1

        simulator: Simulator = Simulator(self.module)
        simulator.add_testbench(drive)
        simulator.run()
        return tested


def clocked(unit: object, process: object) -> None:
    module: Module = Module()
    module.submodules.unit = unit

    simulator: Simulator = Simulator(module)
    simulator.add_clock(1e-6)
    simulator.add_testbench(process)
    simulator.run()


ADDER_OPS: tuple = (AluOp.ADD, AluOp.ADC, AluOp.SUB, AluOp.SBB)
LOGIC_OPS: tuple = (AluOp.AND, AluOp.OR, AluOp.XOR)
ALU_OPS: tuple = ADDER_OPS + LOGIC_OPS + (AluOp.RRC,)

CORNERS: tuple = (0x00, 0x01, 0x02, 0x0F, 0x3F, 0x40, 0x55, 0x7E,
                  0x7F, 0x80, 0x81, 0xAA, 0xC0, 0xF0, 0xFE, 0xFF)
SPARSE: tuple = (0x01, 0x80, 0xFF)


def alu_model(op: int, a: int, b: int, cin: int) -> tuple:
    if (op in ADDER_OPS):
        addend: int = (~b) & 0xFF if (op in (AluOp.SUB, AluOp.SBB)) else b
        if (op == AluOp.ADD):
            carry: int = 0
        elif (op == AluOp.SUB):
            carry = 1
        else:
            carry = cin

        total: int = a + addend + carry
        y: int = total & 0xFF
        signs_agree: bool = ((a ^ addend) & 0x80) == 0
        result_differs: bool = ((a ^ y) & 0x80) != 0
        return (y, int(y == 0), (total >> 8) & 1, (y >> 7) & 1,
                int(signs_agree and result_differs))

    if (op in LOGIC_OPS):
        if (op == AluOp.AND):
            y = a & b
        elif (op == AluOp.OR):
            y = a | b
        else:
            y = a ^ b
        return (y, int(y == 0), 0, (y >> 7) & 1, 0)

    y = (cin << 7) | (a >> 1)
    return (y, int(y == 0), a & 1, (y >> 7) & 1, 0)


def alu_pairs(exhaustive: bool) -> tuple:
    if (exhaustive):
        return tuple((a, b) for a in range(256) for b in range(256))
    return tuple(sorted(
        {(a, b) for a in CORNERS for b in CORNERS}
        | {(a, b) for a in range(256) for b in SPARSE}
        | {(a, b) for a in SPARSE for b in range(256)}
    ))


def test_alu_matches_the_manual_flag_tables(exhaustive: bool) -> None:
    unit: AluUnit = AluUnit()
    sim: CombSim = CombSim(
        unit, (unit.alu_op, unit.a, unit.b, unit.cin), (unit.y, unit.z, unit.c, unit.n, unit.v)
    )
    pairs: tuple = alu_pairs(exhaustive)

    def check(vector: tuple, outputs: list) -> None:
        expected: tuple = alu_model(*vector)
        if (tuple(outputs) != expected):
            op, a, b, cin = vector
            raise AssertionError(
                f"{AluOp(op).name} a={a:#04x} b={b:#04x} cin={cin}: the design produced "
                f"y,z,c,n,v = {tuple(outputs)} and the manual requires {expected}"
            )

    vectors: object = (
        (op, a, b, cin) for op in ALU_OPS for a, b in pairs for cin in (0, 1)
    )
    assert sim.sweep(vectors, check) == len(ALU_OPS) * len(pairs) * 2


def test_subtraction_carry_is_not_a_borrow() -> None:
    assert alu_model(AluOp.SUB, 0x05, 0x03, 0)[2] == 1, "5 - 3 needs no borrow"
    assert alu_model(AluOp.SUB, 0x03, 0x05, 0)[2] == 0, "3 - 5 borrows"

    unit: AluUnit = AluUnit()
    sim: CombSim = CombSim(unit, (unit.alu_op, unit.a, unit.b, unit.cin), (unit.c,))

    def check(vector: tuple, outputs: list) -> None:
        a, b = vector[1], vector[2]
        assert outputs[0] == int(a >= b), f"{a:#04x} - {b:#04x} carry"

    assert sim.sweep(
        ((AluOp.SUB, a, b, 0) for a in CORNERS for b in CORNERS), check
    ) == len(CORNERS) ** 2


def test_rrc_shifts_the_carry_in_and_captures_bit_zero() -> None:
    unit: AluUnit = AluUnit()
    sim: CombSim = CombSim(unit, (unit.alu_op, unit.a, unit.b, unit.cin), (unit.y, unit.c, unit.v))

    def check(vector: tuple, outputs: list) -> None:
        a, cin = vector[1], vector[3]
        y, c, v = outputs
        assert y == ((cin << 7) | (a >> 1)), f"a={a:#04x} cin={cin}: y={y:#04x}"
        assert c == (a & 1), "C takes the shifted-out bit 0 of RS, not the adder carry"
        assert v == 0, "RRC clears V"

    assert sim.sweep(
        ((AluOp.RRC, a, 0x00, cin) for a in range(256) for cin in (0, 1)), check
    ) == 512


RESERVED_CMP: int = 0b111


def cmp_model(encoding: int, z: int, c: int, n: int, v: int) -> int:
    if (encoding == CmpFlag.ALWAYS):
        return 1
    if (encoding == CmpFlag.EQUAL):
        return z
    if (encoding == CmpFlag.NOT_EQUAL):
        return 1 - z
    if (encoding == CmpFlag.BELOW):
        return 1 - c
    if (encoding == CmpFlag.LESS):
        return n ^ v
    if (encoding == CmpFlag.GREATER_EQUAL):
        return 1 - (n ^ v)
    if (encoding == CmpFlag.ABOVE_EQUAL):
        return c
    return None


def test_cmp_matches_the_manual_comparison_table() -> None:
    unit: CmpUnit = CmpUnit()
    sim: CombSim = CombSim(
        unit, (unit.cmp_flag, unit.z, unit.c, unit.n, unit.v), (unit.true,)
    )

    def check(vector: tuple, outputs: list) -> None:
        expected: int = cmp_model(*vector)
        if (expected is None):
            return
        encoding, z, c, n, v = vector
        assert outputs[0] == expected, (
            f"{CmpFlag(encoding).name} with z={z} c={c} n={n} v={v}: the design says "
            f"{outputs[0]} and the manual says {expected}"
        )

    vectors: object = (
        (encoding, z, c, n, v) for encoding in range(8)
        for z in (0, 1) for c in (0, 1) for n in (0, 1) for v in (0, 1)
    )
    assert sim.sweep(vectors, check) == 8 * 16


def test_the_reserved_comparison_cannot_reach_the_cmp_unit() -> None:
    unit: DecoderUnit = DecoderUnit()
    sim: CombSim = CombSim(unit, (unit.inst, unit.extns), (unit.is_illegal,))

    def check(vector: tuple, outputs: list) -> None:
        assert outputs[0] == 1, f"inst={vector[0]:#06x} carries the reserved comparison"

    vectors: object = (
        (op | (n1 << 4) | (n2 << 8) | (RESERVED_CMP << 12) | (top << 15), 0xF)
        for op in (Op.JAL, Op.JMP) for n1 in range(16) for n2 in range(16) for top in (0, 1)
    )
    assert sim.sweep(vectors, check) == 2 * 16 * 16 * 2


DECODER_FIELDS: tuple = (
    "op", "n1", "n2", "n3", "imm4", "imm8", "cmp", "csr",
    "csr_user_readable", "csr_user_writable", "is_jmp_csr", "is_syscall", "is_illegal"
)

CSR_EXTENSIONS: dict = {
    CsrReg.SADDR: 0, CsrReg.RADDR: 0, CsrReg.FLAGS: 0,
    CsrReg.CAUSE: 0, CsrReg.EXTNS: 0, CsrReg.FAULT: 0,
    CsrReg.RETSC: Extn.SYSCALL, CsrReg.RETIR: Extn.INTERRUPT,
    CsrReg.PRIVS: Extn.PRIVILEGE, CsrReg.MPUCF: Extn.PRIVILEGE
}
RESERVED_CSRS: tuple = (0b0000, 0b0111, 0b1100, 0b1101, 0b1110, 0b1111)
USER_READABLE_CSRS: tuple = (CsrReg.SADDR, CsrReg.RADDR, CsrReg.FLAGS, CsrReg.PRIVS)
USER_WRITABLE_CSRS: tuple = (CsrReg.SADDR, CsrReg.RADDR, CsrReg.FLAGS)

GATED_WORDS: tuple = (
    (Op.RSR | (CsrReg.RETSC << 12), Extn.SYSCALL),
    (Op.RSR | (CsrReg.RETIR << 12), Extn.INTERRUPT),
    (Op.RSR | (CsrReg.PRIVS << 12), Extn.PRIVILEGE),
    (Op.RSR | (CsrReg.MPUCF << 12), Extn.PRIVILEGE),
    (Op.WSR | (CsrReg.RETSC << 12), Extn.SYSCALL),
    (Op.WSR | (CsrReg.MPUCF << 12), Extn.PRIVILEGE),
    (Op.JMP | (CsrReg.RETSC << 4) | (CmpFlag.ALWAYS << 12) | (1 << 15), Extn.SYSCALL),
    (Op.JMP | (CsrReg.RETIR << 4) | (CmpFlag.ALWAYS << 12) | (1 << 15), Extn.INTERRUPT),
    (Op.HALT | (1 << 15), Extn.SYSCALL)
)

EXTNS_BLIND_STRIDE: int = 101


def csr_available(csr: int, extns: int) -> bool:
    required: int = CSR_EXTENSIONS.get(csr)
    if (required is None):
        return False
    return (required == 0) or bool(extns & required)


def decode_model(word: int, extns: int) -> dict:
    op: int = word & 0xF
    n1: int = (word >> 4) & 0xF
    n2: int = (word >> 8) & 0xF
    n3: int = (word >> 12) & 0xF
    cmp: int = (word >> 12) & 0b111
    top: int = (word >> 15) & 1

    is_jmp_csr: int = int((op == Op.JMP) and bool(top))
    is_syscall: int = int((op == Op.HALT) and bool(top))
    csr: int = n1 if (is_jmp_csr) else n3

    illegal: bool = False
    if (op == Op.HALT):
        illegal = (((word >> 8) & 0x7F) != 0) or (bool(is_syscall) and not (extns & Extn.SYSCALL))
    elif (op == Op.RRC):
        illegal = n3 != 0
    elif ((op == Op.RSR) or (op == Op.WSR)):
        illegal = not csr_available(csr, extns)
    elif (op == Op.JAL):
        illegal = bool(top) or (cmp == RESERVED_CMP)
    elif (op == Op.JMP):
        illegal = (cmp == RESERVED_CMP) or (
            bool(is_jmp_csr) and ((n2 != 0) or not csr_available(csr, extns))
        )

    return {
        "op": op, "n1": n1, "n2": n2, "n3": n3, "imm4": n3, "imm8": (word >> 8) & 0xFF,
        "cmp": cmp, "csr": csr,
        "csr_user_readable": int(csr in USER_READABLE_CSRS),
        "csr_user_writable": int(csr in USER_WRITABLE_CSRS),
        "is_jmp_csr": is_jmp_csr, "is_syscall": is_syscall, "is_illegal": int(illegal)
    }


def decoder_sim(unit: DecoderUnit, outputs: tuple) -> CombSim:
    return CombSim(unit, (unit.inst, unit.extns), outputs)


def test_decoder_matches_an_independent_model(exhaustive: bool) -> None:
    unit: DecoderUnit = DecoderUnit()
    sim: CombSim = decoder_sim(unit, tuple(getattr(unit, name) for name in DECODER_FIELDS))
    extns_values: tuple = tuple(range(16)) if (exhaustive) else (0xF,)

    def check(vector: tuple, outputs: list) -> None:
        word, extns = vector
        expected: dict = decode_model(word, extns)
        produced: dict = dict(zip(DECODER_FIELDS, outputs))
        if (produced != expected):
            wrong: str = ", ".join(
                f"{name}={produced[name]:#x} where the manual gives {expected[name]:#x}"
                for name in DECODER_FIELDS if (produced[name] != expected[name])
            )
            raise AssertionError(f"inst={word:#06x} extns={extns:#x}: {wrong}")

    vectors: object = ((word, extns) for extns in extns_values for word in range(0x10000))
    assert sim.sweep(vectors, check) == 0x10000 * len(extns_values)


def test_decode_legality_follows_the_enabled_extensions() -> None:
    unit: DecoderUnit = DecoderUnit()
    sim: CombSim = decoder_sim(unit, (unit.is_illegal,))
    legality: dict = {}

    def record(vector: tuple, outputs: list) -> None:
        legality[vector] = outputs[0]

    words: tuple = tuple(word for word, _ in GATED_WORDS)
    assert sim.sweep(
        ((word, extns) for word in words for extns in range(16)), record
    ) == len(words) * 16

    for word, needed in GATED_WORDS:
        for extns in range(16):
            legal: bool = legality[(word, extns)] == 0
            assert legal == bool(extns & needed), (
                f"inst={word:#06x} with extns={extns:#x} should "
                f"{'decode' if (extns & needed) else 'fail to decode'}"
            )


def test_reserved_csr_encodings_never_decode() -> None:
    unit: DecoderUnit = DecoderUnit()
    sim: CombSim = decoder_sim(unit, (unit.is_illegal,))

    def check(vector: tuple, outputs: list) -> None:
        assert outputs[0] == 1, \
            f"inst={vector[0]:#06x} names a reserved special register under extns={vector[1]:#x}"

    vectors: tuple = tuple(
        (word, extns)
        for csr in RESERVED_CSRS
        for word in (Op.RSR | (csr << 12), Op.WSR | (csr << 12),
                     Op.JMP | (csr << 4) | (CmpFlag.ALWAYS << 12) | (1 << 15))
        for extns in range(16)
    )
    assert sim.sweep(vectors, check) == len(RESERVED_CSRS) * 3 * 16


def test_extns_changes_nothing_else_about_a_decode() -> None:
    unit: DecoderUnit = DecoderUnit()
    sim: CombSim = decoder_sim(unit, tuple(getattr(unit, name) for name in DECODER_FIELDS))
    seen: dict = {}

    def record(vector: tuple, outputs: list) -> None:
        seen.setdefault(vector[0], set()).add(tuple(outputs))

    words: tuple = tuple(
        word for word in range(0, 0x10000, EXTNS_BLIND_STRIDE)
        if ((word & 0xF) not in (Op.RSR, Op.WSR))
        and (not ((word & 0xF) in (Op.JMP, Op.HALT) and (word & 0x8000)))
    )
    assert words, "the stride selected no words"
    assert sim.sweep(
        ((word, extns) for word in words for extns in range(16)), record
    ) == len(words) * 16

    for word, decodes in seen.items():
        assert len(decodes) == 1, f"inst={word:#06x} decodes differently depending on EXTNS"


def test_gp_reg_file_names_registers_after_the_abi() -> None:
    unit: GpRegFile = GpRegFile()
    assert tuple(signal.name for signal in unit.gp_regs) == GP_REG_NAMES

    async def process(ctx: object) -> None:
        for slot, name in enumerate(GP_REG_NAMES):
            assert ctx.get(unit.gp_regs[slot]) == 0x00, f"{name} does not reset to zero"

    clocked(unit, process)


def test_gp_reg_file_init_lands_in_the_named_registers() -> None:
    unit: GpRegFile = GpRegFile(GpRegInit(ra = 0x11, rt = 0xD0, rscratch1 = 0xFF))

    async def process(ctx: object) -> None:
        ctx.set(unit.read_gp_reg_a, GpReg.RA)
        ctx.set(unit.read_gp_reg_b, GpReg.RSCRATCH1)
        assert ctx.get(unit.read_data_a) == 0x11
        assert ctx.get(unit.read_data_b) == 0xFF
        assert ctx.get(unit.gp_regs[GpReg.RT]) == 0xD0
        assert ctx.get(unit.gp_regs[GpReg.RB]) == 0x00, "an unnamed register stays zero"

    clocked(unit, process)


def test_gp_reg_file_holds_r0_at_zero() -> None:
    unit: GpRegFile = GpRegFile()

    async def process(ctx: object) -> None:
        ctx.set(unit.write_gp_reg, GpReg.R0)
        ctx.set(unit.write_data, 0xFF)
        ctx.set(unit.write_enable, 1)
        await ctx.tick()
        ctx.set(unit.write_enable, 0)

        ctx.set(unit.read_gp_reg_a, GpReg.R0)
        assert ctx.get(unit.read_data_a) == 0x00, "r0 reads zero"
        assert ctx.get(unit.gp_regs[GpReg.R0]) == 0x00, "r0 ignores writes"

    clocked(unit, process)


def test_gp_reg_file_write_is_visible_after_the_edge() -> None:
    unit: GpRegFile = GpRegFile()

    async def process(ctx: object) -> None:
        ctx.set(unit.read_gp_reg_a, GpReg.RC)
        ctx.set(unit.read_gp_reg_b, GpReg.RC)
        ctx.set(unit.write_gp_reg, GpReg.RC)
        ctx.set(unit.write_data, 0x5A)
        ctx.set(unit.write_enable, 1)
        assert ctx.get(unit.read_data_a) == 0x00, "a read during the write returns the old value"

        await ctx.tick()
        ctx.set(unit.write_enable, 0)
        assert ctx.get(unit.read_data_a) == 0x5A, "both ports see the write afterwards"
        assert ctx.get(unit.read_data_b) == 0x5A

        ctx.set(unit.write_data, 0xA5)
        await ctx.tick()
        assert ctx.get(unit.read_data_a) == 0x5A, "a write needs write_enable"

    clocked(unit, process)


def test_gp_reg_file_read_ports_are_independent() -> None:
    unit: GpRegFile = GpRegFile(GpRegInit(ra = 0x11, rb = 0x22))

    async def process(ctx: object) -> None:
        ctx.set(unit.read_gp_reg_a, GpReg.RA)
        ctx.set(unit.read_gp_reg_b, GpReg.RB)
        assert (ctx.get(unit.read_data_a), ctx.get(unit.read_data_b)) == (0x11, 0x22)

        ctx.set(unit.read_gp_reg_a, GpReg.RB)
        assert (ctx.get(unit.read_data_a), ctx.get(unit.read_data_b)) == (0x22, 0x22)

    clocked(unit, process)


def csr_of(name: str) -> CsrReg:
    return CsrReg[name.upper()]


def test_csr_write_masks_match_the_comparison_table() -> None:
    unit: CsrRegFile = CsrRegFile()

    async def process(ctx: object) -> None:
        for name in CSR_REG_NAMES + ("mpucf",):
            register: CsrReg = csr_of(name)
            ctx.set(unit.write_csr_reg, register)
            ctx.set(unit.write_data, 0xFFFF)
            ctx.set(unit.write_enable, 1)
            await ctx.tick()
            ctx.set(unit.write_enable, 0)

            ctx.set(unit.read_csr_reg, register)
            assert ctx.get(unit.read_data) == CSR_WRITE_MASKS[name], (
                f"{name} read back {ctx.get(unit.read_data):#06x} after a write of 0xFFFF, "
                f"expected {CSR_WRITE_MASKS[name]:#06x}"
            )

    clocked(unit, process)


def test_csr_taps_follow_their_registers() -> None:
    unit: CsrRegFile = CsrRegFile(CsrRegInit(
        flags = 0x000A, cause = 0x1234, extns = 0x000F, fault = 0xBEEF, privs = 0x0001
    ))

    async def process(ctx: object) -> None:
        assert ctx.get(unit.flags_data) == 0x000A
        assert ctx.get(unit.cause_data) == 0x1234
        assert ctx.get(unit.extns_data) == 0x000F
        assert ctx.get(unit.fault_data) == 0xBEEF
        assert ctx.get(unit.privs_data) == 0x0001

    clocked(unit, process)


def test_cause_write_drops_its_reserved_bits() -> None:
    unit: CsrRegFile = CsrRegFile()

    async def process(ctx: object) -> None:
        ctx.set(unit.write_csr_reg, CsrReg.CAUSE)
        ctx.set(unit.write_data, 0x00C0)
        ctx.set(unit.write_enable, 1)
        await ctx.tick()
        ctx.set(unit.write_enable, 0)
        assert ctx.get(unit.cause_data) == 0x0000, "bits 6 and 7 of CAUSE are reserved"

    clocked(unit, process)


def test_extns_bit_zero_ignores_writes() -> None:
    unit: CsrRegFile = CsrRegFile(CsrRegInit(extns = 0x000F))

    async def process(ctx: object) -> None:
        ctx.set(unit.write_csr_reg, CsrReg.EXTNS)
        ctx.set(unit.write_data, 0x0000)
        ctx.set(unit.write_enable, 1)
        await ctx.tick()
        ctx.set(unit.write_enable, 0)
        assert ctx.get(unit.extns_data) == Extn.DYNAMICHW, \
            "the Dynamic Hardware Extension cannot disable itself"

    clocked(unit, process)


def test_extns_writes_are_ignored_without_dynamic_hardware() -> None:
    static: int = Extn.SYSCALL | Extn.PRIVILEGE
    unit: CsrRegFile = CsrRegFile(CsrRegInit(extns = static))

    async def process(ctx: object) -> None:
        ctx.set(unit.write_csr_reg, CsrReg.EXTNS)
        ctx.set(unit.write_data, 0x000F)
        ctx.set(unit.write_enable, 1)
        await ctx.tick()
        ctx.set(unit.write_enable, 0)
        assert ctx.get(unit.extns_data) == static, "static hardware ignores EXTNS writes entirely"

    clocked(unit, process)


def test_mpucf_writes_reach_the_indexed_region() -> None:
    unit: CsrRegFile = CsrRegFile()
    written: tuple = tuple(
        (slot << 3) | ((0xB0 + slot) << 8) for slot in range(MPU_REGION_COUNT)
    )

    async def process(ctx: object) -> None:
        for word in written:
            ctx.set(unit.write_csr_reg, CsrReg.MPUCF)
            ctx.set(unit.write_data, word)
            ctx.set(unit.write_enable, 1)
            await ctx.tick()
            ctx.set(unit.write_enable, 0)

            ctx.set(unit.read_csr_reg, CsrReg.MPUCF)
            assert ctx.get(unit.read_data) == word, "MPUCF reads back the last region written"

        for slot in range(MPU_REGION_COUNT):
            assert ctx.get(unit.mpu_unit.regions[slot]) == written[slot], \
                f"MPUCF.Index should have selected region {slot}"
        assert ctx.get(unit.mpu_unit.index) == MPU_REGION_COUNT - 1

    clocked(unit, process)


def test_mpu_grants_nothing_before_configuration() -> None:
    unit: MpuUnit = MpuUnit()
    sim: CombSim = CombSim(unit, (unit.query_addr,), (unit.query_caps,))

    def check(vector: tuple, outputs: list) -> None:
        assert outputs[0] == 0b000, f"{vector[0]:#06x} is granted {outputs[0]:#05b} after reset"

    assert sim.sweep(
        (((page << 8) | offset,) for page in range(256) for offset in (0x00, 0x7F, 0xFF)), check
    ) == 256 * 3


def test_mpu_region_covers_exactly_its_aligned_pages() -> None:
    for size in range(8):
        for base in (0x00, 0x0A, 0x7F, 0xFF):
            region: int = mpu_region(base, size, 0, CAPS_RWX)
            covered: range = mpu_pages(base, size)
            unit: MpuUnit = MpuUnit(MpuInit(regions = (region, 0x0000, 0x0000, 0x0000), index = 0))
            sim: CombSim = CombSim(unit, (unit.query_addr,), (unit.query_caps,))

            def check(vector: tuple, outputs: list) -> None:
                page: int = vector[0] >> 8
                expected: int = CAPS_RWX if (page in covered) else 0b000
                assert outputs[0] == expected, (
                    f"base={base:#04x} size={size} covers pages "
                    f"{covered.start:#04x}-{covered.stop - 1:#04x}, but page {page:#04x} "
                    f"is granted {outputs[0]:#05b}"
                )

            assert sim.sweep((((page << 8) | 0x40,) for page in range(256)), check) == 256
            assert covered.stop <= 0x100, "no region may extend past page 0xFF"


def test_mpu_retains_the_base_bits_it_ignores() -> None:
    region: int = mpu_region(0x0A, 3, 0, CAPS_RWX)
    unit: MpuUnit = MpuUnit(MpuInit(regions = (region, 0x0000, 0x0000, 0x0000), index = 0))

    async def process(ctx: object) -> None:
        assert ctx.get(unit.read_data) == region, "the written word is read back unchanged"
        ctx.set(unit.query_addr, 0x0800)
        assert ctx.get(unit.query_caps) == CAPS_RWX, "the region starts at page 0x08"
        ctx.set(unit.query_addr, 0x0700)
        assert ctx.get(unit.query_caps) == 0b000, "and not at page 0x0A"

    clocked(unit, process)


def test_mpu_lowest_index_wins_whatever_the_write_order() -> None:
    lower: int = mpu_region(0x20, 0, 1, 0b100)
    higher: int = mpu_region(0x20, 0, 2, 0b010)

    for order in ((lower, higher), (higher, lower)):
        unit: MpuUnit = MpuUnit()

        async def process(ctx: object) -> None:
            for word in order:
                ctx.set(unit.write_data, word)
                ctx.set(unit.write_enable, 1)
                await ctx.tick()
                ctx.set(unit.write_enable, 0)

            ctx.set(unit.query_addr, 0x2000)
            assert ctx.get(unit.query_caps) == 0b100, (
                f"region 1 should win over region 2 when written "
                f"{'first' if (order[0] == lower) else 'second'}"
            )

        clocked(unit, process)
