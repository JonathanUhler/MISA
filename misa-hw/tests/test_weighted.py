from dataclasses import replace

from pytest import MarkDecorator, mark, raises

from misa_hw.grammar import CmpFlag, Extn, GpReg

from compare import compared
from generators import Item, Knobs, PROFILES, Program, SUBROUTINE_BASE, weighted_program
from memory import MemoryLatency
from programs import PROGRAM_BASE, jal_inst, set_inst
from spec import SpecDriver
from target import Target


pytestmark: MarkDecorator = mark.random


def profile_label(value: object) -> str:
    return value.name if (isinstance(value, Knobs)) else str(value)


def test_every_profile_describes_a_program_which_can_be_built() -> None:
    names: set = set()
    for knobs in PROFILES:
        assert knobs.name not in names, f"two profiles are called {knobs.name}"
        names.add(knobs.name)
        assert knobs.purpose, f"the {knobs.name} profile does not say what it is for"

        program: Program = weighted_program(knobs, 0)
        main: tuple = program.main_words()
        assert PROGRAM_BASE + 2 * len(main) < SUBROUTINE_BASE, (
            f"the {knobs.name} program is {len(main)} words and runs into the subroutines"
        )
        assert len(program.body) == knobs.length
        assert len(program.subroutines) == knobs.call_depth
        assert program.steps > program.length(), "the step cap has to allow the whole program"


@mark.parametrize("knobs", PROFILES, ids = profile_label)
def test_a_profile_produces_the_instruction_classes_it_asks_for(knobs: Knobs) -> None:
    kinds: set = set()
    for seed in range(8):
        kinds |= set(weighted_program(knobs, seed).kinds())

    assert ("alu" in kinds) and ("set" in kinds), "every profile computes something"
    assert ("call" in kinds) == bool(knobs.call_depth)
    assert ("syscall" in kinds) == knobs.syscalls
    assert ("mpu" in kinds) == bool(knobs.weights()["mpu"])
    assert ("extns" in kinds) == bool(knobs.weights()["extns"])
    assert bool(kinds & set(knobs.faults())) == bool(knobs.fault_probability), (
        f"the {knobs.name} profile asks for a fault probability of {knobs.fault_probability} "
        f"and produced the classes {sorted(kinds)}"
    )


@mark.parametrize("knobs, problem", [
    (dict(user_mode = True, extns = Extn.SYSCALL), "Privilege Extension disabled"),
    (dict(syscalls = True, extns = Extn.PRIVILEGE), "System Call Extension disabled"),
    (dict(call_depth = 2, stack = False), "nowhere to save its return address")
], ids = ["user-without-privilege", "syscall-without-syscall", "nested-without-stack"])
def test_a_profile_which_contradicts_itself_is_refused(knobs: dict, problem: str) -> None:
    with raises(ValueError, match = problem):
        Knobs("impossible", "a profile which cannot be built", **knobs)


def test_a_generated_program_is_the_same_program_every_time() -> None:
    first: Program = weighted_program(PROFILES[0], 11)
    assert first == weighted_program(PROFILES[0], 11), "a seed has to name one program forever"
    assert first != weighted_program(PROFILES[0], 12)


def test_a_branch_resolves_to_the_item_it_jumps_to() -> None:
    knobs: Knobs = Knobs("branchy", "one forward branch over two instructions", length = 0)
    body: tuple = (
        Item("set", (set_inst(GpReg.RA, 0x01),)),
        Item("branch", (0x0000, 0x0000,
                        jal_inst(CmpFlag.ALWAYS, GpReg.RSCRATCH0, GpReg.RSCRATCH1)),
             target = 3),
        Item("set", (set_inst(GpReg.RB, 0x02),)),
        Item("set", (set_inst(GpReg.RC, 0x03),))
    )
    program: Program = replace(weighted_program(knobs, 0), body = body)

    offsets, end = program.offsets()
    words: tuple = program.main_words()
    landing: int = PROGRAM_BASE + 2 * offsets[3]
    assert words[offsets[1]] == set_inst(GpReg.RSCRATCH0, (landing >> 8) & 0xFF)
    assert words[offsets[1] + 1] == set_inst(GpReg.RSCRATCH1, landing & 0xFF)
    assert landing < PROGRAM_BASE + 2 * end, "a branch only ever jumps forward"


def test_a_branch_past_the_body_lands_on_the_epilogue() -> None:
    knobs: Knobs = Knobs("branchy", "a branch out of the body", length = 0)
    body: tuple = (
        Item("branch", (0x0000, 0x0000,
                        jal_inst(CmpFlag.ALWAYS, GpReg.RSCRATCH0, GpReg.RSCRATCH1)),
             target = 1),
    )
    program: Program = replace(weighted_program(knobs, 0), body = body)

    offsets, end = program.offsets()
    landing: int = PROGRAM_BASE + 2 * end
    assert program.main_words()[offsets[0] + 1] == set_inst(GpReg.RSCRATCH1, landing & 0xFF), (
        "a target one past the last item is the epilogue, which is where a branch at the end of "
        "the body has to go"
    )


@mark.subset
@mark.parametrize("knobs", PROFILES, ids = profile_label)
def test_a_weighted_program_runs_the_same_way(target: Target, spec: SpecDriver,
                                              latency: MemoryLatency, compare_mode: str,
                                              knobs: Knobs, profile_seed: int) -> None:
    program: Program = weighted_program(knobs, profile_seed)
    states: list = compared(compare_mode, target, spec, program.image(), program.memmap,
                            steps = program.steps, extns = knobs.extns, latency = latency)

    assert states[-1].halted, (
        f"the {knobs.name} program of seed {profile_seed} did not halt in {program.steps} "
        f"instructions. Its control flow only moves forward, so it should have reached either "
        f"its own HALT or the one in the fault handler"
    )
