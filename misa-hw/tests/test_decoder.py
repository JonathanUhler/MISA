from amaranth.sim import Simulator, SimulatorContext

from misa_hw.decoder import Decoder

import golden


_FIELDS: tuple = (
    "op", "n1", "n2", "n3", "imm8", "imm4", "cmp", "csr_target", "is_syscall", "illegal"
)


def test_decoder_exhaustive() -> None:
    dut: Decoder = Decoder()
    mismatches: list = []

    async def tb(ctx: SimulatorContext) -> None:
        for word in range(0x10000):
            ctx.set(dut.inst, word)
            got: dict = {name: int(ctx.get(getattr(dut.out, name))) for name in _FIELDS}
            exp: dict = golden.decode(word)
            if (got != exp):
                mismatches.append((word, got, exp))
                if (len(mismatches) >= 10):
                    return

    sim: Simulator = Simulator(dut)
    sim.add_testbench(tb)
    sim.run()

    assert mismatches == [], f"decoder mismatch (first {len(mismatches)}): {mismatches[:5]}"
