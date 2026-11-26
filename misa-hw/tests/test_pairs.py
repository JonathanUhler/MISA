from pytest import mark, raises, skip

from misa_hw.grammar import GpReg, Op
from misa_hw.init import CoreInit

from compare import DivergenceError, steps_from
from generators import (
    CAMPAIGN_MEMMAP, DEFAULT_EXTNS, PAIR_SEEDS, Seed, SeedProgram, compatible_seeds,
    first_instructions, instruction_pairs, seed_label, seed_program, second_instructions,
    selected_seeds
)
from memory import MemMap, MemoryLatency
from programs import PAGE_SIZE, PROGRAM_BASE, patched, set_inst, word_bytes
from spec import SpecDriver
from state import MisaState
from target import RunResult, Target


PAIR_MEMMAP: MemMap = CAMPAIGN_MEMMAP
PAIR_STEPS: int = 2

FIRSTS: tuple = first_instructions()
SECONDS: tuple = second_instructions()
ALPHABET_MINIMUM: int = 100
ALPHABET_MAXIMUM: int = 300

PAIR_GRID: tuple = compatible_seeds((DEFAULT_EXTNS,), PAIR_SEEDS)

ALU_OPS: tuple = (Op.ADD, Op.ADC, Op.SUB, Op.SBB, Op.AND, Op.OR, Op.XOR, Op.RRC)
CARRY_OPS: tuple = (Op.ADC, Op.SBB, Op.RRC)

label: object = seed_label


def pair_slots(program: SeedProgram) -> tuple:
    second: int = program.slot + 2
    if (second + 1 >= PROGRAM_BASE + PAGE_SIZE):
        raise ValueError(
            f"the {program.seed.name} prologue pushes the second instruction of a pair to "
            f"{second:#06x}, outside the page at {PROGRAM_BASE:#06x}"
        )
    return (program.slot, second)


def pair_label(program: SeedProgram, first: int, second: int) -> str:
    return (f"{program.seed.name} ({program.seed.purpose}): the pair {first:#06x} {second:#06x} "
            f"executed at {program.slot:#06x}")


def fields(words: tuple, op: int, shift: int, mask: int, top: int = None) -> set:
    return {
        (word >> shift) & mask for word in words
        if ((word & 0xF) == op) and ((top is None) or (((word >> 15) & 1) == top))
    }


def sweep_pairs(target: Target, spec: SpecDriver, program: SeedProgram, entry: MisaState,
                extns: int, pairs: tuple, latency: MemoryLatency) -> int:
    init: CoreInit = entry.to_core_init()
    low, high = pair_slots(program)
    jobs: tuple = tuple(
        ((low, word_bytes(first)), (high, word_bytes(second))) for first, second in pairs
    )
    buffer: bytearray = bytearray(program.image)
    tested: int = 0

    records: object = spec.sweep_chunks(program.image, PAIR_MEMMAP, extns, jobs,
                                        steps = program.presteps + PAIR_STEPS,
                                        interrupts = program.interrupts)
    for (first, second), (job, expected) in zip(pairs, records):
        for addr, data in job:
            if (expected.memory[addr:addr + 2] != data):
                raise AssertionError(
                    f"the oracle record for the job patching {data.hex()} at {addr:#06x} does "
                    f"not carry those bytes, so either records and jobs are no longer aligned or "
                    f"the pair stored over itself, which no seed puts an address near enough to "
                    f"{PROGRAM_BASE:#06x} to do"
                )
            buffer[addr:addr + 2] = data

        result: RunResult = target.run(bytes(buffer), init, PAIR_STEPS, (), PAIR_MEMMAP, latency)
        steps_from(pair_label(program, first, second), entry, expected, result,
                   steps = PAIR_STEPS)
        tested += 1

    return tested


def test_the_alphabets_are_the_size_they_were_planned_at() -> None:
    for name, words in (("first", FIRSTS), ("second", SECONDS)):
        assert len(words) == len(set(words)), f"the {name} alphabet repeats an encoding"
        assert ALPHABET_MINIMUM <= len(words) <= ALPHABET_MAXIMUM, (
            f"the {name} alphabet is {len(words)} instructions, which is not the size the pair "
            f"campaign was planned at"
        )


def test_the_first_alphabet_produces_every_dependency_a_pair_can_carry() -> None:
    assert {word & 0xF for word in FIRSTS} == set(range(16)), "every opcode leads a pair"

    for op in ALU_OPS:
        assert fields(FIRSTS, op, 4, 0xF), f"{Op(op).name} is there to produce flags"
    assert fields(FIRSTS, Op.WSR, 12, 0xF) == set(range(16)), "every CSR is written"
    assert GpReg.RSCRATCH1 in fields(FIRSTS, Op.LD, 8, 0xF), "a stack load updates SADDR"
    assert GpReg.RSCRATCH1 in fields(FIRSTS, Op.ST, 8, 0xF), "a stack store updates SADDR"
    assert fields(FIRSTS, Op.JAL, 12, 0b111) == set(range(8)), "a JAL links for the second"
    assert fields(FIRSTS, Op.HALT, 15, 0b1) == {0, 1}, "both HALT and SYSCALL lead a pair"


def test_the_second_alphabet_consumes_every_dependency_a_pair_can_carry() -> None:
    for op in CARRY_OPS:
        assert fields(SECONDS, op, 4, 0xF), f"{Op(op).name} consumes the carry the first left"
    assert fields(SECONDS, Op.JAL, 12, 0b111) == set(range(8)), "every comparison consumes FLAGS"
    assert fields(SECONDS, Op.JMP, 12, 0b111, top = 0) == set(range(8))
    assert fields(SECONDS, Op.JMP, 12, 0b111, top = 1) == set(range(8)), "through a CSR as well"
    assert fields(SECONDS, Op.RSR, 12, 0xF) == set(range(16)), "every CSR is read back"
    assert fields(SECONDS, Op.JMP, 4, 0xF, top = 1) == set(range(16)), "and jumped through"

    for op in (Op.LD, Op.ST):
        assert fields(SECONDS, op, 8, 0xF) >= {0x0, 0x1, 0xE, 0xF}, "every addressing mode"
        assert fields(SECONDS, op, 12, 0xF) >= {0x0, 0x7, 0x8, 0xF}, "both signs and extremes"


@mark.parametrize("seed", PAIR_SEEDS, ids = label)
def test_a_pair_fits_the_page_the_seed_grants_itself(seed: Seed) -> None:
    program: SeedProgram = seed_program(seed)
    low, high = pair_slots(program)
    assert low == program.slot
    assert high == low + 2
    assert high + 1 < PROGRAM_BASE + PAGE_SIZE


@mark.slow
@mark.parametrize("seed, extns", PAIR_GRID, ids = label)
def test_every_pair_matches_the_oracle(target: Target, spec: SpecDriver, seeds: object,
                                       latency: MemoryLatency, exhaustive: bool, seed_count: int,
                                       seed: Seed, extns: int) -> None:
    if (seed not in selected_seeds(exhaustive, seed_count, PAIR_SEEDS)):
        skip(f"{seed.name} is not in the selected seeds; use --exhaustive or --seeds")

    program, entry = seeds(seed, extns)
    pairs: tuple = instruction_pairs(FIRSTS, SECONDS)
    assert sweep_pairs(target, spec, program, entry, extns, pairs, latency) == len(pairs)


def test_a_pair_sweep_builds_the_design_once(target: Target, spec: SpecDriver, seeds: object,
                                             latency: MemoryLatency) -> None:
    program, entry = seeds(PAIR_SEEDS[0], DEFAULT_EXTNS)
    pairs: tuple = instruction_pairs(FIRSTS[:8], SECONDS[:8])
    builds: int = target.builds

    assert sweep_pairs(target, spec, program, entry, DEFAULT_EXTNS, pairs, latency) == len(pairs)
    assert target.builds - builds <= 1, \
        "a pair sweep which rebuilds the design per pair is ~60x slower and still passes"


def test_the_second_instruction_is_the_one_that_ran(target: Target, spec: SpecDriver,
                                                    seeds: object) -> None:
    program, entry = seeds(PAIR_SEEDS[0], DEFAULT_EXTNS)
    low, high = pair_slots(program)
    first: int = set_inst(GpReg.RA, 0x42)
    asked: int = set_inst(GpReg.RB, 0x17)
    answered: int = set_inst(GpReg.RB, 0x18)

    jobs: tuple = (((low, word_bytes(first)), (high, word_bytes(asked))),)
    _, finals = spec.sweep(program.image, PAIR_MEMMAP, DEFAULT_EXTNS, jobs,
                           steps = program.presteps + PAIR_STEPS)
    assert finals[0].gp_regs[GpReg.RA] == 0x42, "the first instruction of the pair ran"
    assert finals[0].gp_regs[GpReg.RB] == 0x17, "and so did the second"

    image: bytes = patched(program.image, low, word_bytes(first), word_bytes(answered))
    result: RunResult = target.run(image, entry.to_core_init(), PAIR_STEPS, (), PAIR_MEMMAP)

    with raises(DivergenceError, match = "rb"):
        steps_from("a mismatched pair", entry, finals[0], result, steps = PAIR_STEPS)


def test_a_pair_which_halts_first_is_not_a_short_trace(target: Target, spec: SpecDriver,
                                                       seeds: object) -> None:
    program, entry = seeds(PAIR_SEEDS[0], DEFAULT_EXTNS)
    low, high = pair_slots(program)
    halting: int = 0x0000
    second: int = set_inst(GpReg.RB, 0x17)

    jobs: tuple = (((low, word_bytes(halting)), (high, word_bytes(second))),)
    _, finals = spec.sweep(program.image, PAIR_MEMMAP, DEFAULT_EXTNS, jobs,
                           steps = program.presteps + PAIR_STEPS)
    assert finals[0].halted, "the first instruction of this pair is a HALT"

    image: bytes = patched(program.image, low, word_bytes(halting), word_bytes(second))
    result: RunResult = target.run(image, entry.to_core_init(), PAIR_STEPS, (), PAIR_MEMMAP)

    assert len(result.states) == 2, "a halt ends the pair one state early on both sides"
    steps_from("a pair which halts first", entry, finals[0], result, steps = PAIR_STEPS)


def test_a_short_trace_without_a_halt_is_a_divergence(target: Target, spec: SpecDriver,
                                                      seeds: object) -> None:
    program, entry = seeds(PAIR_SEEDS[0], DEFAULT_EXTNS)
    low, high = pair_slots(program)
    image: bytes = patched(program.image, low, word_bytes(set_inst(GpReg.RA, 0x42)),
                           word_bytes(set_inst(GpReg.RB, 0x17)))

    result: RunResult = target.run(image, entry.to_core_init(), 1, (), PAIR_MEMMAP)
    assert not result.halted, "neither instruction of this pair halts"

    with raises(DivergenceError, match = "produced 2 states for 2 instructions"):
        steps_from("a truncated run", entry, result.states[-1], result, steps = PAIR_STEPS)
