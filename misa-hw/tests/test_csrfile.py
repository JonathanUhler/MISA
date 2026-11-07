import random

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.csrfile import CsrFile
from misa_hw.init import CsrInit
from misa_hw.grammar import Csr, Extn

import golden


def _sim(dut: CsrFile, tb) -> None:
    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()


def test_write_read_each_csr() -> None:
    dut: CsrFile = CsrFile(init_data = CsrInit(extns = 0x0F))
    writes: list = [(Csr.SADDR, 0x1234), (Csr.RADDR, 0xBEEF), (Csr.CAUSE, 0xCAFE),
                    (Csr.RETSC, 0x1111), (Csr.RETIR, 0x2222), (Csr.FLAGS, 0x0A), (Csr.PRIVS, 0x1)]
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        for reg, val in writes:
            ctx.set(dut.write_reg, reg)
            ctx.set(dut.write_data, val)
            ctx.set(dut.write_enable, 1)
            await ctx.tick()
        ctx.set(dut.write_enable, 0)
        vals: dict = {}
        for reg, _ in writes:
            ctx.set(dut.read_reg, reg)
            vals[int(reg)] = int(ctx.get(dut.read_data))
        out["vals"] = vals

    _sim(dut, tb)
    assert out["vals"][int(Csr.SADDR)] == 0x1234
    assert out["vals"][int(Csr.RADDR)] == 0xBEEF
    assert out["vals"][int(Csr.CAUSE)] == 0xCAFE
    assert out["vals"][int(Csr.RETSC)] == 0x1111
    assert out["vals"][int(Csr.RETIR)] == 0x2222
    assert out["vals"][int(Csr.FLAGS)] == 0x0A    # FLAGS is 4-bit
    assert out["vals"][int(Csr.PRIVS)] == 0x1     # PRIVS is 1-bit


def test_flags_dedicated_write_wins() -> None:
    dut: CsrFile = CsrFile(init_data = CsrInit(extns = 0x0F))
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.write_reg, Csr.FLAGS)
        ctx.set(dut.write_data, 0x5)
        ctx.set(dut.write_enable, 1)
        ctx.set(dut.flags_write_enable, 1)
        ctx.set(dut.flags_write_data, 0xA)
        await ctx.tick()
        ctx.set(dut.write_enable, 0)
        ctx.set(dut.flags_write_enable, 0)
        ctx.set(dut.read_reg, Csr.FLAGS)
        out["read"] = int(ctx.get(dut.read_data))
        out["flags_data"] = int(ctx.get(dut.flags_data))

    _sim(dut, tb)
    assert out["read"] == 0xA         # the dedicated flags write overrides the general FLAGS write
    assert out["flags_data"] == 0xA


def test_extns_write_ignored_when_dynamichw_off() -> None:
    dut: CsrFile = CsrFile(init_data = CsrInit(extns = 0x00))
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.write_reg, Csr.EXTNS)
        ctx.set(dut.write_data, 0x0F)
        ctx.set(dut.write_enable, 1)
        await ctx.tick()
        ctx.set(dut.write_enable, 0)
        ctx.set(dut.read_reg, Csr.EXTNS)
        out["extns"] = int(ctx.get(dut.read_data))

    _sim(dut, tb)
    assert out["extns"] == 0x00       # Dynamic Hardware off -> EXTNS writes ignored


def test_extns_bit0_pinned_when_dynamichw_on() -> None:
    dut: CsrFile = CsrFile(init_data = CsrInit(extns = Extn.DYNAMICHW))
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.write_reg, Csr.EXTNS)
        ctx.set(dut.write_data, 0b1110)   # try to clear bit 0, set bits 1-3
        ctx.set(dut.write_enable, 1)
        await ctx.tick()
        ctx.set(dut.write_enable, 0)
        ctx.set(dut.read_reg, Csr.EXTNS)
        out["extns"] = int(ctx.get(dut.read_data))

    _sim(dut, tb)
    assert out["extns"] == 0b1111     # bit 0 stays pinned, bits 1-3 take the written value


def test_fuzz_against_model() -> None:
    extns0: int = 0x0F
    dut: CsrFile = CsrFile(init_data = CsrInit(extns = extns0))
    model: golden.CsrModel = golden.CsrModel(extns = extns0)
    rng: random.Random = random.Random(7)
    regs: list = [Csr.SADDR, Csr.RADDR, Csr.FLAGS, Csr.CAUSE, Csr.EXTNS, Csr.RETSC, Csr.RETIR,
                  Csr.PRIVS]
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        bad: list = []
        for _ in range(400):
            wr: int = rng.choice(regs)
            wd: int = rng.randint(0, 0xFFFF)
            we: int = rng.randint(0, 1)
            fwe: int = rng.randint(0, 1)
            fwd: int = rng.randint(0, 0xF)
            ctx.set(dut.write_reg, wr)
            ctx.set(dut.write_data, wd)
            ctx.set(dut.write_enable, we)
            ctx.set(dut.flags_write_enable, fwe)
            ctx.set(dut.flags_write_data, fwd)
            await ctx.tick()
            model.step(int(wr), we, wd, fwe, fwd)
            ctx.set(dut.write_enable, 0)
            ctx.set(dut.flags_write_enable, 0)
            for reg in regs:
                ctx.set(dut.read_reg, reg)
                got: int = int(ctx.get(dut.read_data))
                exp: int = model.read(int(reg))
                if (got != exp):
                    bad.append((int(reg), got, exp))
        out["bad"] = bad

    _sim(dut, tb)
    assert out["bad"] == [], f"csr fuzz mismatch (reg,got,exp): {out['bad'][:10]}"
