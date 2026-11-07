import random

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.grammar import Reg
from misa_hw.regfile import RegFile


def _sim(dut: RegFile, tb) -> None:
    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()


def test_r0_reads_zero_and_ignores_writes() -> None:
    dut: RegFile = RegFile()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.read_reg, Reg.R0)
        out["read"] = int(ctx.get(dut.read_data))
        ctx.set(dut.write_reg, Reg.R0)
        ctx.set(dut.write_data, 0xAB)
        ctx.set(dut.write_enable, 1)
        await ctx.tick()
        ctx.set(dut.write_enable, 0)
        ctx.set(dut.read_reg, Reg.R0)
        out["after_write"] = int(ctx.get(dut.read_data))

    _sim(dut, tb)
    assert out["read"] == 0
    assert out["after_write"] == 0


def test_write_then_read_each_register() -> None:
    dut: RegFile = RegFile()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        for r in range(1, 16):
            ctx.set(dut.write_reg, r)
            ctx.set(dut.write_data, (r * 7) & 0xFF)
            ctx.set(dut.write_enable, 1)
            await ctx.tick()
        ctx.set(dut.write_enable, 0)
        vals: dict = {}
        for r in range(1, 16):
            ctx.set(dut.read_reg, r)
            vals[r] = int(ctx.get(dut.read_data))
        out["vals"] = vals

    _sim(dut, tb)
    for r in range(1, 16):
        assert out["vals"][r] == (r * 7) & 0xFF, f"reg {r}"


def test_write_enable_low_is_noop() -> None:
    dut: RegFile = RegFile()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.write_reg, 1)
        ctx.set(dut.write_data, 0x55)
        ctx.set(dut.write_enable, 1)
        await ctx.tick()
        ctx.set(dut.write_data, 0x99)
        ctx.set(dut.write_enable, 0)
        await ctx.tick()
        ctx.set(dut.read_reg, 1)
        out["v"] = int(ctx.get(dut.read_data))

    _sim(dut, tb)
    assert out["v"] == 0x55


def test_fuzz_against_shadow() -> None:
    dut: RegFile = RegFile()
    rng: random.Random = random.Random(5)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        shadow: dict = {i: 0 for i in range(16)}
        bad: list = []
        for _ in range(500):
            wr: int = rng.randint(0, 15)
            data: int = rng.randint(0, 255)
            we: int = rng.randint(0, 1)
            ctx.set(dut.write_reg, wr)
            ctx.set(dut.write_data, data)
            ctx.set(dut.write_enable, we)
            await ctx.tick()
            if we and wr != 0:
                shadow[wr] = data
            ctx.set(dut.write_enable, 0)
            rr: int = rng.randint(0, 15)
            ctx.set(dut.read_reg, rr)
            got: int = int(ctx.get(dut.read_data))
            exp: int = 0 if rr == 0 else shadow[rr]
            if got != exp:
                bad.append((rr, got, exp))
        out["bad"] = bad

    _sim(dut, tb)
    assert out["bad"] == [], f"regfile fuzz mismatch (reg,got,exp): {out['bad'][:10]}"
