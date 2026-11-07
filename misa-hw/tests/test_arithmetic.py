import random

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.arithmetic import ArithmeticUnit
from misa_hw.grammar import ArithmeticOp

import golden


_SAMPLES: int = 10000


def _vectors(exhaustive: bool):
    if (exhaustive):
        for op in ArithmeticOp:
            for a in range(256):
                for b in range(256):
                    for cin in (0, 1):
                        yield (op, a, b, cin)
    else:
        rng: random.Random = random.Random(0xA10)
        for _ in range(_SAMPLES):
            yield (rng.choice(tuple(ArithmeticOp)),
                   rng.randint(0, 255),
                   rng.randint(0, 255),
                   rng.randint(0, 1))


def test_alu_equivalence(exhaustive: bool) -> None:
    dut: ArithmeticUnit = ArithmeticUnit()
    mismatches: list = []

    async def tb(ctx: SimulatorContext) -> None:
        for op, a, b, cin in _vectors(exhaustive):
            ctx.set(dut.op, op)
            ctx.set(dut.a, a)
            ctx.set(dut.b, b)
            ctx.set(dut.cin, cin)
            got: tuple = (int(ctx.get(dut.y)), int(ctx.get(dut.z)), int(ctx.get(dut.c)),
                          int(ctx.get(dut.n)), int(ctx.get(dut.v)))
            exp: tuple = golden.alu(op, a, b, cin)
            if (got != exp):
                mismatches.append((op, a, b, cin, got, exp))
                if (len(mismatches) >= 10):
                    return

    sim: Simulator = Simulator(dut)
    sim.add_testbench(tb)
    sim.run()

    assert mismatches == [], f"alu mismatch (op,a,b,cin,got,exp): {mismatches[:10]}"
