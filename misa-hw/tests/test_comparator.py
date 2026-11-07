from itertools import product

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.comparator import ComparatorUnit

import golden


def test_comparator_exhaustive() -> None:
    dut: ComparatorUnit = ComparatorUnit()
    mismatches: list = []

    async def tb(ctx: SimulatorContext) -> None:
        for cmp in range(8):
            for z, c, n, v in product((0, 1), repeat = 4):
                ctx.set(dut.cmp, cmp)
                ctx.set(dut.z, z)
                ctx.set(dut.c, c)
                ctx.set(dut.n, n)
                ctx.set(dut.v, v)
                got: int = int(ctx.get(dut.taken))
                exp: int = golden.comparator(cmp, z, c, n, v)
                if (got != exp):
                    mismatches.append((cmp, z, c, n, v, got, exp))

    sim: Simulator = Simulator(dut)
    sim.add_testbench(tb)
    sim.run()

    assert mismatches == [], f"comparator mismatch (cmp,z,c,n,v,got,exp): {mismatches[:10]}"
