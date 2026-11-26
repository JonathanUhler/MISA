from dataclasses import replace

from pytest import mark, raises, skip

from misa_hw.grammar import CsrReg, FaultKind, GpReg, Op
from misa_hw.init import CoreInit

from compare import DivergenceError, lockstep, single_step
from generators import (
    ALL_ENCODINGS, CAMPAIGN_MEMMAP, DEFAULT_EXTNS, HALT_RESERVED, HANDLER_BASE, NIBBLE_RESERVED,
    SEEDS, Seed, SeedProgram, compatible_seeds, representative_encodings, seed_label,
    seed_problems, seed_program, selected_seeds
)
from memory import MemMap, MemoryLatency
from programs import PAGE_SIZE, PROGRAM_BASE, patched, set_inst, word_bytes
from spec import SpecDriver
from state import MisaState
from target import RunResult, Target


SWEEP_MEMMAP: MemMap = CAMPAIGN_MEMMAP

SWEEP_EXTNS: tuple = (0xF, 0x0)
QUICK_EXTNS: tuple = (0xF,)

REPRESENTATIVE: tuple = representative_encodings()
REPRESENTATIVE_MINIMUM: int = 1000
REPRESENTATIVE_MAXIMUM: int = 3000

EXTNS_VALUES: tuple = tuple(range(1 << 4))
EXTNS_CROSS_SEED: Seed = SEEDS[1]

label: object = seed_label

FULL_GRID: tuple = compatible_seeds(SWEEP_EXTNS)
REPRESENTATIVE_GRID: tuple = compatible_seeds(QUICK_EXTNS) + ((SEEDS[0], 0x0),)


def sweep_label(program: SeedProgram, extns: int, word: int) -> str:
    return (f"{program.seed.name} ({program.seed.purpose}) extns={extns:#x}: encoding "
            f"{word:#06x} executed at {program.slot:#06x}")


def sweep_encodings(target: Target, spec: SpecDriver, program: SeedProgram, entry: MisaState,
                    extns: int, encodings: object, latency: MemoryLatency) -> int:
    init: CoreInit = entry.to_core_init()
    jobs: tuple = tuple(((program.slot, word_bytes(word)),) for word in encodings)
    buffer: bytearray = bytearray(program.image)
    tested: int = 0

    for job, expected in spec.sweep_chunks(program.image, SWEEP_MEMMAP, extns, jobs,
                                           steps = program.presteps + 1,
                                           interrupts = program.interrupts):
        addr, data = job[0]
        if (expected.memory[addr:addr + 2] != data):
            raise AssertionError(
                f"the oracle record for the job patching {data.hex()} at {addr:#06x} does not "
                f"carry those bytes, so records and jobs are no longer aligned"
            )

        buffer[addr:addr + 2] = data
        result: RunResult = target.run(bytes(buffer), init, 1, (), SWEEP_MEMMAP, latency)
        single_step(sweep_label(program, extns, data[0] | (data[1] << 8)),
                    entry, expected, result)
        tested += 1

    return tested


@mark.parametrize("seed", SEEDS, ids = label)
def test_a_seed_program_fits_the_page_it_grants_itself(seed: Seed) -> None:
    program: SeedProgram = seed_program(seed)
    assert program.slot == PROGRAM_BASE + 2 * len(seed.prologue())
    assert program.slot + 1 < PROGRAM_BASE + PAGE_SIZE
    assert program.presteps == len(seed.prologue()) + len(program.interrupts)


def test_a_prologue_which_leaves_its_page_is_refused() -> None:
    sprawling: Seed = Seed("too-long", "a prologue which cannot fit in one page",
                           mpu = (0x0000,) * 50)
    with raises(ValueError, match = "outside the page"):
        seed_program(sprawling)


@mark.subset
@mark.parametrize("seed", SEEDS, ids = label)
def test_a_seed_prologue_runs_the_same_way_on_both_sides(target: Target, spec: SpecDriver,
                                                         latency: MemoryLatency,
                                                         seed: Seed) -> None:
    program: SeedProgram = seed_program(seed)
    states: list = lockstep(target, spec, program.image, SWEEP_MEMMAP,
                            steps = len(seed.prologue()), extns = DEFAULT_EXTNS,
                            latency = latency)

    assert not states[-1].halted, "the prologue should not have halted"
    assert states[-1].pc == program.slot, \
        "every path through a prologue ends at the instruction under test"
    if (not seed.interrupt):
        assert seed_problems(program, states[-1]) == [], \
            "the design reaches the same seed state the oracle does"


@mark.subset
@mark.parametrize("seed", SEEDS, ids = label)
def test_an_injected_seed_resumes_the_way_the_oracle_resumes_it(target: Target, spec: SpecDriver,
                                                                seeds: object,
                                                                latency: MemoryLatency,
                                                                seed: Seed) -> None:
    program, entry = seeds(seed, DEFAULT_EXTNS)
    expected: MisaState = spec.qi(entry, SWEEP_MEMMAP, steps = 1)[-1]
    result: RunResult = target.run(program.image, entry.to_core_init(), 1, (), SWEEP_MEMMAP,
                                   latency)

    single_step(f"{seed.name}: the HALT an unpatched slot holds", entry, expected, result)
    if (seed.privs):
        assert not expected.halted, "HALT is a privileged instruction in User Mode"
        assert expected.masked_csr("cause") == FaultKind.PRIV_INSTRUCTION
        assert expected.pc == HANDLER_BASE, "a refused HALT enters the fault handler"
    else:
        assert expected.halted, "an unpatched slot holds a HALT"


@mark.parametrize("seed", SEEDS, ids = label)
def test_a_seed_description_notices_the_wrong_state(seeds: object, seed: Seed) -> None:
    program, entry = seeds(seed, DEFAULT_EXTNS)
    assert seed_problems(program, entry) == []

    moved: MisaState = replace(entry, pc = entry.pc + 2)
    assert ("pc", program.slot, program.slot + 2) in seed_problems(program, moved), \
        "the seed description has to notice a state the prologue did not produce"


@mark.subset
@mark.parametrize("seed, extns", REPRESENTATIVE_GRID, ids = label)
def test_representative_encodings_match_the_oracle(target: Target, spec: SpecDriver,
                                                   seeds: object, latency: MemoryLatency,
                                                   seed: Seed, extns: int) -> None:
    program, entry = seeds(seed, extns)
    tested: int = sweep_encodings(target, spec, program, entry, extns, REPRESENTATIVE, latency)
    assert tested == len(REPRESENTATIVE)


@mark.slow
@mark.parametrize("seed, extns", FULL_GRID, ids = label)
def test_every_encoding_matches_the_oracle(target: Target, spec: SpecDriver, seeds: object,
                                           latency: MemoryLatency, exhaustive: bool,
                                           seed_count: int, seed: Seed, extns: int) -> None:
    if (seed not in selected_seeds(exhaustive, seed_count)):
        skip(f"{seed.name} is not in the selected seeds; use --exhaustive or --seeds")
    if (extns not in (SWEEP_EXTNS if (exhaustive) else QUICK_EXTNS)):
        skip(f"extns={extns:#x} is only swept under --exhaustive")

    program, entry = seeds(seed, extns)
    tested: int = sweep_encodings(target, spec, program, entry, extns, ALL_ENCODINGS, latency)
    assert tested == len(ALL_ENCODINGS)


@mark.exhaustive
@mark.slow
@mark.parametrize("extns", EXTNS_VALUES, ids = label)
def test_representative_encodings_match_under_every_extension_set(target: Target,
                                                                  spec: SpecDriver,
                                                                  seeds: object,
                                                                  latency: MemoryLatency,
                                                                  extns: int) -> None:
    program, entry = seeds(EXTNS_CROSS_SEED, extns)
    tested: int = sweep_encodings(target, spec, program, entry, extns, REPRESENTATIVE, latency)
    assert tested == len(REPRESENTATIVE)


def test_a_sweep_builds_the_design_once(target: Target, spec: SpecDriver, seeds: object,
                                        latency: MemoryLatency) -> None:
    program, entry = seeds(SEEDS[0], DEFAULT_EXTNS)
    builds: int = target.builds
    sweep_encodings(target, spec, program, entry, DEFAULT_EXTNS, range(0x100), latency)
    assert target.builds - builds <= 1, \
        "a sweep which rebuilds the design per encoding is ~60x slower and still passes"


def test_a_chunked_sweep_answers_the_same_jobs(spec: SpecDriver) -> None:
    program: SeedProgram = seed_program(SEEDS[1])
    encodings: tuple = (0x0000, 0x1234, 0xA1F2, 0xBFF2, 0xC123, 0xE123, 0xF8A3)
    jobs: tuple = tuple(((program.slot, word_bytes(word)),) for word in encodings)
    steps: int = program.presteps + 1

    _, finals = spec.sweep(program.image, SWEEP_MEMMAP, DEFAULT_EXTNS, jobs, steps = steps)
    chunked: list = [
        state for _, state in spec.sweep_chunks(program.image, SWEEP_MEMMAP, DEFAULT_EXTNS,
                                                jobs, steps = steps, chunk = 3)
    ]
    assert chunked == list(finals)
    assert len({state.gp_regs for state in chunked}) > 1, "these encodings should not all agree"


def test_a_chunk_has_to_make_progress(spec: SpecDriver) -> None:
    with raises(ValueError, match = "cannot make progress"):
        list(spec.sweep_chunks(bytes(0x10000), SWEEP_MEMMAP, DEFAULT_EXTNS, (), chunk = 0))


def test_the_comparison_notices_the_wrong_encoding(target: Target, spec: SpecDriver,
                                                   seeds: object) -> None:
    program, entry = seeds(SEEDS[1], DEFAULT_EXTNS)
    asked: int = set_inst(GpReg.RA, 0x42)
    answered: int = set_inst(GpReg.RA, 0x43)

    jobs: tuple = (((program.slot, word_bytes(asked)),),)
    _, finals = spec.sweep(program.image, SWEEP_MEMMAP, DEFAULT_EXTNS, jobs,
                           steps = program.presteps + 1)

    image: bytes = patched(program.image, program.slot, word_bytes(answered))
    result: RunResult = target.run(image, entry.to_core_init(), 1, (), SWEEP_MEMMAP)

    with raises(DivergenceError, match = "ra"):
        single_step("a mismatched pair", entry, finals[0], result)


def test_representative_encodings_reach_every_cell() -> None:
    words: tuple = REPRESENTATIVE
    assert len(words) == len(set(words)), "the reduced set should not repeat an encoding"
    assert REPRESENTATIVE_MINIMUM <= len(words) <= REPRESENTATIVE_MAXIMUM, \
        f"the reduced set is {len(words)} encodings, which is not the size it was planned at"

    assert {word & 0xF for word in words} == set(range(16)), "every opcode has a representative"

    def fields(op: int, shift: int, mask: int, top: int = None) -> set:
        return {
            (word >> shift) & mask for word in words
            if ((word & 0xF) == op) and ((top is None) or (((word >> 15) & 1) == top))
        }

    assert fields(Op.HALT, 15, 0b1) == {0, 1}, "both HALT and SYSCALL appear"
    assert fields(Op.HALT, 8, 0x7F) == set(HALT_RESERVED), "HALT sweeps its reserved field"
    assert fields(Op.RRC, 12, 0xF) == set(NIBBLE_RESERVED), "RRC sweeps its reserved field"
    assert fields(Op.JAL, 15, 0b1) == {0, 1}, "a JAL with its reserved bit set appears"

    for op in (Op.RSR, Op.WSR):
        assert fields(op, 12, 0xF) == set(range(16)), f"{Op(op).name} names every CSR encoding"
    assert fields(Op.JMP, 4, 0xF, top = 1) == set(range(16)), "JMP names every CSR encoding"
    assert fields(Op.JMP, 12, 0b111, top = 0) == set(range(8))
    assert fields(Op.JMP, 12, 0b111, top = 1) == set(range(8))
    assert fields(Op.JAL, 12, 0b111) == set(range(8)), "every comparison appears"

    for op in (Op.LD, Op.ST):
        bases: set = fields(op, 8, 0xF)
        assert 0xF in bases, f"{Op(op).name} has a stack variant"
        assert 0xE in bases, f"{Op(op).name} has a pair which wraps in four bits"
        assert 0x0 in bases, f"{Op(op).name} has a pair whose high byte is R0"
        assert fields(op, 12, 0xF) >= {0x0, 0x7, 0x8, 0xF}, "both offset signs and both extremes"

    assert fields(Op.SET, 8, 0xFF) >= {0x00, 0xFF}, "SET covers its immediate extremes"
    assert GpReg.R0 in fields(Op.ADD, 4, 0xF), "an ALU result discarded into R0 appears"
    assert CsrReg.MPUCF in fields(Op.WSR, 12, 0xF), "the MPU window is written somewhere"
