from pathlib import Path

from pytest import fixture, mark, raises

from misa_hw.grammar import AccessKind

from compare import DivergenceError, check_lengths, final, lockstep
from conftest import BRINGUP_DIR
from memory import (
    CAPS_RWX, CAPS_READ, CAPS_WRITE, caps_from_name, caps_to_name,
    MemoryLatency, ZeroLatency, FixedLatency, RandomLatency,
    MemMap, MemoryBfm
)
from spec import SpecDriver, split_records
from state import MisaState
from target import RunResult, Target


FIRSTTEST: Path = BRINGUP_DIR / "bin" / "firsttest.bin"
MEMTEST: Path = BRINGUP_DIR / "bin" / "memtest.bin"

MEMTEST_STEPS: int = 200

MEMTEST_FAULT_STEP: int = 10
MEMTEST_FAULT_PC: int = 0xC012
MEMTEST_HANDLER_PC: int = 0xC046


@fixture(scope = "session")
def firsttest_image() -> bytes:
    return FIRSTTEST.read_bytes()


@fixture(scope = "session")
def memtest_image() -> bytes:
    return MEMTEST.read_bytes()


@mark.parametrize("name, bits", [
    ("rwx", CAPS_RWX), ("r", CAPS_READ), ("rw", CAPS_READ | CAPS_WRITE), ("-", 0)
])
def test_caps_names_round_trip(name: str, bits: int) -> None:
    assert caps_from_name(name) == bits
    assert caps_to_name(bits) == name


def test_memmap_rejects_empty() -> None:
    with raises(ValueError):
        MemMap(())


def test_memmap_unmapped_is_not_zero_caps() -> None:
    memmap: MemMap = MemMap.from_rows(((0x0000, 0x00FF, "rwx"), (0x0200, 0x02FF, "-")))
    assert memmap.caps_at(0x0000) == CAPS_RWX
    assert memmap.caps_at(0x0200) == 0
    assert memmap.caps_at(0x0100) is None


def test_memmap_first_region_wins() -> None:
    memmap: MemMap = MemMap.from_rows(((0x0000, 0xFFFF, "r"), (0x0000, 0x00FF, "rwx")))
    assert memmap.caps_at(0x0010) == CAPS_READ


def test_memmap_state_section_coalesces_runs() -> None:
    memmap: MemMap = MemMap.from_rows(((0x0000, 0x7FFF, "rwx"), (0x8000, 0xFFFF, "rwx")))
    assert memmap.to_state_file_section() == "  0x0000 - 0xffff : rwx\n"


def test_memmap_state_section_round_trip() -> None:
    memmap: MemMap = MemMap.from_rows((
        (0x0000, 0x00FF, "rw"), (0x0200, 0x7FFF, "rwx"), (0xC000, 0xFFFF, "rx")
    ))
    rows: list = [
        tuple(part.strip() for part in line.split(":"))
        for line in memmap.to_state_file_section().strip().split("\n")
    ]
    rebuilt: MemMap = MemMap.from_state_file_rows(tuple(rows))
    for addr in (0x0000, 0x00FF, 0x0100, 0x0200, 0x7FFF, 0x8000, 0xBFFF, 0xC000, 0xFFFF):
        assert rebuilt.caps_at(addr) == memmap.caps_at(addr), f"at {addr:#06x}"


def test_state_file_round_trip_is_byte_identical(spec: SpecDriver, firsttest_image: bytes,
                                                 everything: MemMap) -> None:
    emitted: str = spec.state_file_text(firsttest_image, everything, 0xF)
    state, memmap = MisaState.from_spec_text(emitted)
    assert state.to_spec_file(memmap) == emitted


def test_state_file_requires_the_memory_map(spec: SpecDriver, firsttest_image: bytes) -> None:
    narrow: MemMap = MemMap.from_rows(((0xC000, 0xFFFF, "rx"),))
    state, _ = MisaState.from_spec_text(spec.state_file_text(firsttest_image, narrow, 0xF))
    _, recovered = MisaState.from_spec_text(state.to_spec_file(narrow))
    assert recovered == narrow
    assert recovered.caps_at(0x0000) is None


def test_mpucf_is_derived_not_stored(spec: SpecDriver, firsttest_image: bytes,
                                     everything: MemMap) -> None:
    state, _ = MisaState.from_spec_text(spec.state_file_text(firsttest_image, everything, 0xF))
    assert state.mpucf == state.mpu_regions[state.mpu_index]


def test_split_records_keeps_empty_records() -> None:
    assert split_records("a\n---\n---\nb") == ["a", "", "b"]


def test_delta_replay_matches_a_direct_run(spec: SpecDriver, firsttest_image: bytes,
                                           everything: MemMap) -> None:
    traced: list = spec.q0(firsttest_image, everything, 0xF, trace = True)
    assert len(traced) > 2, "firsttest should take more than one step"

    for steps in range(len(traced)):
        direct: MisaState = spec.q0(firsttest_image, everything, 0xF, steps = steps)[-1]
        assert traced[steps] == direct, f"folded state {steps} does not match a direct run"


def test_bfm_drives_responses_without_a_request(everything: MemMap) -> None:
    bfm: MemoryBfm = MemoryBfm(bytes(0x10000), MemMap.from_rows(((0x0000, 0x00FF, "r"),)))
    assert bfm.responses(0x0000) == (1, CAPS_READ)
    assert bfm.responses(0x0100) == (0, 0b000)


def test_bfm_latency_holds_ready_then_answers() -> None:
    image: bytearray = bytearray(0x10000)
    image[0x0040] = 0xA5
    bfm: MemoryBfm = MemoryBfm(bytes(image), MemMap.everything(), FixedLatency(3))

    for cycle in range(3):
        assert bfm.settle(cycle, 0x0040, True, False, 0) == (0, 0), f"ready too early at {cycle}"
    assert bfm.settle(3, 0x0040, True, False, 0) == (1, 0xA5)


def test_bfm_latency_restarts_when_the_address_changes() -> None:
    bfm: MemoryBfm = MemoryBfm(bytes(0x10000), MemMap.everything(), FixedLatency(2))
    assert bfm.settle(0, 0x0100, True, False, 0) == (0, 0)
    assert bfm.settle(1, 0x0100, True, False, 0) == (0, 0)
    assert bfm.settle(2, 0x0100, True, False, 0)[0] == 1
    assert bfm.settle(3, 0x0200, True, False, 0) == (0, 0), "second request answered too early"


def test_bfm_journals_writes_and_snapshots_share_until_written() -> None:
    bfm: MemoryBfm = MemoryBfm(bytes(0x10000), MemMap.everything())
    before: bytes = bfm.snapshot()
    assert bfm.snapshot() is before, "an unwritten snapshot should not be copied again"

    bfm.settle(0, 0x0300, True, True, 0x5A)
    after: bytes = bfm.snapshot()
    assert bfm.writes == [(0x0300, 0x5A)]
    assert after[0x0300] == 0x5A
    assert before[0x0300] == 0x00, "an earlier snapshot must not change under a later write"


def test_boundary_count_matches_the_oracle(target: Target, spec: SpecDriver,
                                           firsttest_image: bytes, everything: MemMap,
                                           latency: MemoryLatency) -> None:
    oracle: list = spec.q0(firsttest_image, everything, 0xF, trace = True)
    result: RunResult = target.run(firsttest_image, None, None, (), everything, latency,
                                   reset_extns = 0xF)
    assert len(result.states) == len(oracle)
    assert result.states[-1].halted, "firsttest ends in HALT"


@mark.parametrize("profile", [ZeroLatency(), FixedLatency(1), FixedLatency(7), RandomLatency(1, 5)],
                  ids = lambda profile: profile.name)
def test_boundaries_are_independent_of_memory_latency(target: Target, firsttest_image: bytes,
                                                      everything: MemMap,
                                                      profile: MemoryLatency) -> None:
    reference: RunResult = target.run(firsttest_image, None, None, (), everything,
                                      ZeroLatency(), reset_extns = 0xF)
    stalled: RunResult = target.run(firsttest_image, None, None, (), everything, profile,
                                    reset_extns = 0xF)
    assert stalled.states == reference.states
    if (profile.name != "zero"):
        assert stalled.cycles > reference.cycles, "a stall profile should take longer"


def test_check_lengths_rejects_a_short_design_trace() -> None:
    short: RunResult = RunResult([_halted_state(False)], None, 99)
    with raises(DivergenceError, match = "produced 1 states and the oracle 5"):
        check_lengths([_halted_state(False)] * 5, short, 4)


def test_check_lengths_accepts_equal_traces() -> None:
    equal: RunResult = RunResult([_halted_state(False)] * 5, None, 99)
    check_lengths([_halted_state(False)] * 5, equal, 4)


def _halted_state(halted: bool) -> MisaState:
    return MisaState(
        gp_regs = (0,) * 16, csrs = {}, mpu_regions = (0,) * 4, mpu_index = 0, pc = 0,
        in_syscall = False, in_interrupt = False, int_pending = False, int_rscratch = 0,
        int_flags = 0, syscall_privs = 0, int_privs = 0, halted = halted, memory = b""
    )


@mark.subset
def test_lockstep_on_a_bringup_program(target: Target, spec: SpecDriver,
                                       firsttest_image: bytes, everything: MemMap,
                                       latency: MemoryLatency) -> None:
    states: list = lockstep(target, spec, firsttest_image, everything, extns = 0xF,
                            latency = latency)
    assert states[-1].halted
    assert states[0].pc == 0xC000, "the reset vector should have been honoured"


@mark.subset
def test_lockstep_on_a_resumed_run(target: Target, spec: SpecDriver, firsttest_image: bytes,
                                   everything: MemMap, latency: MemoryLatency) -> None:
    from_reset: list = spec.q0(firsttest_image, everything, 0xF, trace = True)
    resumed: list = lockstep(target, spec, firsttest_image, everything, state = from_reset[0],
                             latency = latency)
    assert resumed == from_reset, "a resumed run should reproduce the run it resumed from"


@mark.subset
def test_injection_bypasses_the_reset_vector(target: Target, spec: SpecDriver,
                                             firsttest_image: bytes,
                                             everything: MemMap) -> None:
    image: bytearray = bytearray(firsttest_image)
    image[0xFFFE], image[0xFFFF] = 0xFF, 0xFF

    state, _ = MisaState.from_spec_text(spec.state_file_text(firsttest_image, everything, 0xF))
    result: RunResult = target.run(bytes(image), state.to_core_init(), None, (), everything)

    assert result.states[0].pc == 0xC000, "injection should start at init.pc, not the vector"
    assert result.states[-1].halted
    assert 0xFFFE not in result.bfm.addresses_touched(), "the reset vector was read anyway"


@mark.subset
def test_lockstep_on_a_program_that_stores_and_branches(target: Target, spec: SpecDriver,
                                                        memtest_image: bytes,
                                                        everything: MemMap,
                                                        latency: MemoryLatency) -> None:
    states: list = lockstep(target, spec, memtest_image, everything,
                            steps = MEMTEST_STEPS, extns = 0xF, latency = latency)
    assert len(states) == MEMTEST_STEPS + 1

    written: list = [
        addr for addr in range(0x10000)
        if (states[0].memory[addr] != states[-1].memory[addr])
    ]
    assert written, "the prefix should have stored something, exercising the writes: path"
    assert len({state.masked_csr("flags") for state in states}) > 1, "no flags were produced"


@mark.subset
def test_fault_entry_is_one_step(target: Target, spec: SpecDriver, memtest_image: bytes,
                                 latency: MemoryLatency) -> None:
    narrow: MemMap = MemMap.from_rows(((0xC000, 0xFFFF, "rwx"),))
    steps: int = MEMTEST_FAULT_STEP + 2
    states: list = lockstep(target, spec, memtest_image, narrow, steps = steps, extns = 0xF,
                            latency = latency)

    before: MisaState = states[MEMTEST_FAULT_STEP - 1]
    after: MisaState = states[MEMTEST_FAULT_STEP]
    assert before.pc == MEMTEST_FAULT_PC, "the faulting store should be the one at this pc"
    assert after.pc == MEMTEST_HANDLER_PC, "one step should land in the handler"
    assert after.csrs["fault"] == MEMTEST_FAULT_PC, "FAULT records the faulting instruction"
    assert after.masked_csr("cause") != 0, "the fault should have set a cause"

    result: RunResult = target.run(memtest_image, None, steps, (), narrow, latency,
                                   reset_extns = 0xF)
    touched: set = {addr for addr in result.bfm.addresses_touched() if (addr >= 0xFFF0)}
    assert touched == {0xFFFC, 0xFFFD, 0xFFFE, 0xFFFF}, \
        f"only the reset and fault vectors should have been read, saw {sorted(touched)}"


def test_final_comparison_agrees_with_lockstep(target: Target, spec: SpecDriver,
                                               memtest_image: bytes,
                                               everything: MemMap) -> None:
    states: list = lockstep(target, spec, memtest_image, everything, steps = 40, extns = 0xF)
    ended: MisaState = final(target, spec, memtest_image, everything, steps = 40, extns = 0xF)
    assert ended == states[-1]


def test_reset_arguments_must_name_one_source_of_extns(target: Target, spec: SpecDriver,
                                                       firsttest_image: bytes,
                                                       everything: MemMap) -> None:
    state, _ = MisaState.from_spec_text(spec.state_file_text(firsttest_image, everything, 0xF))
    with raises(ValueError, match = "needs an explicit extns"):
        lockstep(target, spec, firsttest_image, everything)
    with raises(ValueError, match = "takes EXTNS from the state"):
        lockstep(target, spec, firsttest_image, everything, state = state, extns = 0xF)


def test_sweep_records_are_relative_to_the_unpatched_image(spec: SpecDriver,
                                                           firsttest_image: bytes,
                                                           everything: MemMap) -> None:
    encodings: tuple = (0x00D0, 0x11D9, 0x0000, 0x00D0)
    jobs: tuple = tuple(
        ((0xC000, bytes((encoding & 0xFF, encoding >> 8))),) for encoding in encodings
    )
    reference, finals = spec.sweep(firsttest_image, everything, 0xF, jobs, steps = 1)
    assert len(finals) == len(jobs)

    for job, encoding, produced in zip(jobs, encodings, finals):
        addr, data = job[0]
        assert produced.memory[addr:addr + len(data)] == data, "patch bytes missing from record"

        patched: bytearray = bytearray(firsttest_image)
        patched[addr:addr + len(data)] = data
        expected: MisaState = spec.q0(bytes(patched), everything, 0xF, steps = 1)[-1]
        assert produced == expected, f"sweep disagrees with a direct run for {encoding:#06x}"

    assert reference.memory == firsttest_image
    assert finals[0] == finals[-1]


@mark.subset
def test_lockstep_detects_an_injected_fault(target: Target, spec: SpecDriver,
                                            firsttest_image: bytes,
                                            everything: MemMap) -> None:
    corrupted: bytearray = bytearray(firsttest_image)
    corrupted[0xC000] ^= 0xFF

    oracle: list = spec.q0(firsttest_image, everything, 0xF, trace = True)
    design: RunResult = target.run(bytes(corrupted), None, len(oracle) - 1, (), everything,
                                   reset_extns = 0xF)
    differences: list = [
        state.diff(produced) for state, produced in zip(oracle, design.states)
    ]
    assert any(differences), "corrupting an instruction produced no observable difference"
