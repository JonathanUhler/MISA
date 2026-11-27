from dataclasses import dataclass, field, replace

from pytest import MarkDecorator, mark, raises

from misa_hw.grammar import AccessKind, CmpFlag, CsrReg, Extn, FaultKind, GpReg, Op

from compare import Directed, EVERYTHING, expect_fault, handler_entries, handler_index
from generators import (
    DEFAULT_EXTNS, HANDLER_BASE, ILLEGAL_WORD, INTERRUPT_BASE, POP_OFFSET, PUSH_OFFSET,
    STACK_PAIR, STACK_RESET, SYSCALL_BASE, USER_MODE, USER_MODE_WORDS, directed_image
)
from memory import FixedLatency, MemMap, MemoryLatency, ZeroLatency, memmap_with
from programs import (
    FAULT_TAG, INTERRUPT_VECTOR, PAGE_SIZE, PROGRAM_BASE, addr_of, alu_inst, bringup_image,
    cause_word, csr_write_words, expect_finish, flags_word, halt_inst, jal_inst, jmp_csr_inst,
    load_inst, rrc_inst, rsr_inst, set_inst, store_inst, syscall_inst
)
from spec import SpecDriver
from state import BOUNDARY_IGNORED, MisaState
from target import (
    INT_PIN_FIRST, INT_PIN_LEAD, IntPulse, RunResult, Target, pin_for_step, pin_schedule
)


pytestmark: MarkDecorator = mark.subset

NO_INTERRUPT: int = DEFAULT_EXTNS & ~Extn.INTERRUPT

SECOND_BASE: int = PROGRAM_BASE + PAGE_SIZE

HELD_CYCLES: int = 1 << 16

RESET_HOLD: int = 4

HALT_LINGER: int = 16

HANDLER_MARK: int = 0x5A
CAUSE_MARK: int = cause_word(FaultKind.ILLEGAL, 0xA5)
SCRATCH_HIGH: int = 0xAB
SCRATCH_LOW: int = 0xCD
SCRATCH_FLAGS: int = flags_word(z = 0, c = 1, n = 0, v = 1)             # 0xab + 0xcd

SCRATCH_HANDLER: tuple = (
    alu_inst(Op.XOR, GpReg.RSCRATCH0, GpReg.RSCRATCH0, GpReg.RSCRATCH0),
    set_inst(GpReg.RSCRATCH1, HANDLER_MARK),
    jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETIR)
)

MARKING_HANDLER: tuple = (
    set_inst(GpReg.RSCRATCH0, HANDLER_MARK),
    store_inst(GpReg.RSCRATCH0, STACK_PAIR, PUSH_OFFSET),
    load_inst(GpReg.RSCRATCH1, STACK_PAIR, POP_OFFSET),
    jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETIR)
)

READ_CAUSE_AND_HALT: tuple = (
    rsr_inst(CsrReg.CAUSE, GpReg.RU, GpReg.RV), halt_inst(GpReg.RV)
)


@dataclass(frozen = True)
class PinProgram:

    name: str
    purpose: str
    words: tuple
    steps: int
    handler: tuple = SCRATCH_HANDLER
    blocks: dict = None
    memmap: MemMap = field(default_factory = MemMap.everything)
    extns: int = DEFAULT_EXTNS


    def image(self) -> bytes:
        blocks: dict = {INTERRUPT_BASE: self.handler}
        blocks.update(self.blocks or {})
        return directed_image(self.words, blocks = blocks)


    def limit(self, taken: int = 1) -> int:
        return self.steps + taken * (len(self.handler) + 1) + 1


CALIBRATION: PinProgram = PinProgram(
    "calibration", "eight identical register writes, so every instruction costs the same",
    words = tuple(set_inst(GpReg.RA, value) for value in range(1, 9)) + (halt_inst(GpReg.RA),),
    steps = 9
)

STRAIGHT: PinProgram = PinProgram(
    "straight", "arithmetic whose flags are live across the interrupt",
    words = (
        set_inst(GpReg.RA, 0xFF),
        set_inst(GpReg.RB, 0x01),
        alu_inst(Op.ADD, GpReg.RC, GpReg.RA, GpReg.RB),
        alu_inst(Op.SUB, GpReg.RD, GpReg.RA, GpReg.RB),
        rrc_inst(GpReg.RE, GpReg.RA),
        alu_inst(Op.ADC, GpReg.RF, GpReg.RA, GpReg.RB),
        halt_inst(GpReg.RC)
    ),
    steps = 7
)

STACK: PinProgram = PinProgram(
    "stack", "a stack discipline the handler takes part in",
    words = csr_write_words(CsrReg.SADDR, STACK_RESET) + (
        set_inst(GpReg.RA, 0x11),
        store_inst(GpReg.RA, STACK_PAIR, PUSH_OFFSET),
        set_inst(GpReg.RA, 0x22),
        store_inst(GpReg.RA, STACK_PAIR, PUSH_OFFSET),
        load_inst(GpReg.RB, STACK_PAIR, POP_OFFSET),
        load_inst(GpReg.RC, STACK_PAIR, POP_OFFSET),
        halt_inst(GpReg.RB)
    ),
    steps = 10,
    handler = MARKING_HANDLER
)

SYSCALL: PinProgram = PinProgram(
    "syscall", "a system call, so three of its boundaries are inside another context",
    words = (
        set_inst(GpReg.RA, 0x07),
        syscall_inst(GpReg.RA),
        set_inst(GpReg.RB, 0x08),
        halt_inst(GpReg.RB)
    ),
    steps = 6,
    blocks = {SYSCALL_BASE: (
        rsr_inst(CsrReg.CAUSE, GpReg.RU, GpReg.RV), jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RETSC)
    )}
)

FAULTING: PinProgram = PinProgram(
    "faulting", "an instruction which faults, for the interrupt to preempt",
    words = (set_inst(GpReg.RA, 0x5A), ILLEGAL_WORD, halt_inst(GpReg.RA)),
    steps = 4,
    blocks = {HANDLER_BASE: READ_CAUSE_AND_HALT}
)

USER: PinProgram = PinProgram(
    "user", "User Mode, so the entry clears the ring bit and the return restores it",
    words = USER_MODE_WORDS + (
        set_inst(GpReg.RA, 0x21),
        alu_inst(Op.ADD, GpReg.RB, GpReg.RA, GpReg.RA),
        syscall_inst(GpReg.RA)
    ),
    steps = 10,
    blocks = {SYSCALL_BASE: (halt_inst(GpReg.RA),)}
)

SCRATCHED: PinProgram = PinProgram(
    "scratched", "non-zero scratch registers and flags for the entry to save",
    words = (
        set_inst(GpReg.RSCRATCH0, SCRATCH_HIGH),
        set_inst(GpReg.RSCRATCH1, SCRATCH_LOW),
        alu_inst(Op.ADD, GpReg.RA, GpReg.RSCRATCH0, GpReg.RSCRATCH1),
        set_inst(GpReg.RB, 0x01),
        halt_inst(GpReg.RA)
    ),
    steps = 5
)

BRANCHES: PinProgram = PinProgram(
    "branches", "a call and return through RADDR and the scratch pair the handler clobbers",
    words = (
        set_inst(GpReg.RSCRATCH0, SECOND_BASE >> 8),
        set_inst(GpReg.RSCRATCH1, SECOND_BASE & 0xFF),
        jal_inst(CmpFlag.ALWAYS, GpReg.RSCRATCH0, GpReg.RSCRATCH1),
        halt_inst(GpReg.RA)
    ),
    steps = 6,
    blocks = {SECOND_BASE: (
        set_inst(GpReg.RA, 0x77), jmp_csr_inst(CmpFlag.ALWAYS, CsrReg.RADDR)
    )}
)

PIN_PROGRAMS: tuple = (STRAIGHT, STACK, SYSCALL, FAULTING, USER, SCRATCHED, BRANCHES)
PIN_CASES: tuple = tuple(
    (program, step) for program in PIN_PROGRAMS for step in range(program.steps)
)

CYCLE_PROFILES: tuple = (ZeroLatency(), FixedLatency(2))

PIN_ANCHOR: int = 2


def program_label(value: object) -> str:
    if (isinstance(value, (PinProgram, MemoryLatency))):
        return value.name
    if (isinstance(value, str)):
        return value
    if (value is None):
        return "fault"
    return f"step{value}"


def landing(states: list, handler: int = INTERRUPT_BASE) -> int:
    for index, state in enumerate(states):
        if (state.pc == handler):
            return index - 1
    return None


def landed_on(boundary_cycles: tuple, rise: int) -> int:
    seen: int = rise + INT_PIN_LEAD
    for boundary, cycle in enumerate(boundary_cycles):
        if (cycle > seen):
            return boundary
    return None


@mark.parametrize("program", PIN_PROGRAMS + (CALIBRATION,), ids = program_label)
def test_a_program_halts_in_the_steps_it_claims(directed: Directed,
                                                program: PinProgram) -> None:
    states: list = directed.oracle(program.image(), program.memmap, steps = program.limit(0),
                                   extns = program.extns)

    assert len(states) - 1 == program.steps, (
        f"the {program.name} program ({program.purpose}) takes {len(states) - 1} steps to halt "
        f"and the campaign interrupts it at each of the {program.steps} it claims; the claim is "
        f"what bounds which oracle steps this campaign fires at"
    )
    assert states[-1].halted
    assert landing(states) is None, "no program of this campaign takes an interrupt on its own"


def calibrate(target: Target, profile: MemoryLatency, pin: tuple) -> RunResult:
    return target.run(CALIBRATION.image(), None, CALIBRATION.limit(), pin, EVERYTHING, profile,
                      reset_extns = DEFAULT_EXTNS)


@mark.parametrize("profile", CYCLE_PROFILES, ids = program_label)
def test_the_first_cycle_of_a_run_cannot_produce_a_pin_edge(target: Target,
                                                            profile: MemoryLatency) -> None:
    swallowed: RunResult = calibrate(target, profile, (IntPulse(None, 0),))
    earliest: RunResult = calibrate(target, profile, (IntPulse(None, INT_PIN_FIRST),))

    assert landing(swallowed.states) is None, (
        "the synchroniser initialises to 0b111, so the first cycle of a run supplies both bits "
        "the edge detector reads and a pin asserted during it produces no edge"
    )
    assert landing(earliest.states) == 0, (
        "the second cycle is the earliest a pin assertion can be seen from, and it lands on the "
        "boundary the reset sequence produces, which is what pin_for_step(0) is"
    )
    assert pin_for_step(0) == IntPulse(None, INT_PIN_FIRST)


@mark.parametrize("profile", CYCLE_PROFILES, ids = program_label)
def test_the_window_in_which_a_pin_assertion_lands_on_a_boundary(target: Target,
                                                                 profile: MemoryLatency) -> None:
    reference: RunResult = calibrate(target, profile, ())
    cycles: tuple = reference.boundary_cycles

    assert len(cycles) == CALIBRATION.steps, "one boundary per instruction, and no interrupt"
    assert landing(reference.states) is None

    anchor: int = cycles[PIN_ANCHOR]
    span: int = cycles[PIN_ANCHOR + 2] - anchor + INT_PIN_LEAD
    landings: dict = {}

    for after in range(span):
        driven: RunResult = calibrate(target, profile, (IntPulse(PIN_ANCHOR, after),))
        landings[after] = landing(driven.states)
        assert landings[after] == landed_on(cycles, anchor + after), (
            f"a pin assertion {after} cycles after boundary {PIN_ANCHOR} (cycle {anchor + after}) "
            f"landed on boundary {landings[after]}, where int_pending is set at the end of cycle "
            f"{anchor + after + INT_PIN_LEAD} and the boundaries of this run are at {cycles}"
        )

    for boundary in (PIN_ANCHOR + 1, PIN_ANCHOR + 2):
        first: int = cycles[boundary - 1] - INT_PIN_LEAD - anchor
        last: int = cycles[boundary] - INT_PIN_LEAD - 1 - anchor
        window: tuple = tuple(
            after for after in range(span) if (landings[after] == boundary)
        )
        assert window == tuple(range(max(first, 0), last + 1)), (
            f"the assertions which land on boundary {boundary} should be exactly the cycles from "
            f"two before boundary {boundary - 1} to three before boundary {boundary}"
        )

    canonical: int = 0
    opens: int = cycles[PIN_ANCHOR] - INT_PIN_LEAD - anchor
    closes: int = cycles[PIN_ANCHOR + 1] - INT_PIN_LEAD - 1 - anchor

    assert pin_for_step(PIN_ANCHOR + 1) == IntPulse(PIN_ANCHOR, canonical)
    assert landings[canonical] == PIN_ANCHOR + 1
    assert opens == -INT_PIN_LEAD, (
        "the window opens two cycles before the boundary it is anchored to, which are cycles the "
        "run has already left, so the canonical assertion cannot sit at its lower edge"
    )
    assert closes > canonical, (
        "and it closes three cycles before the next boundary, which is later than the canonical "
        "assertion because an instruction is never shorter than four cycles"
    )


def test_a_pin_schedule_which_cannot_fire_is_refused(target: Target) -> None:
    unreachable: IntPulse = IntPulse(CALIBRATION.steps + 1)
    with raises(RuntimeError, match = "never raised the pin"):
        calibrate(target, ZeroLatency(), (unreachable,))


def test_an_oracle_step_is_not_a_pin_schedule(target: Target) -> None:
    with raises(TypeError, match = "is not an IntPulse"):
        calibrate(target, ZeroLatency(), (3,))


@mark.parametrize("program, step", PIN_CASES, ids = program_label)
def test_an_interrupt_arrives_at_the_boundary_the_oracle_fires_at(directed: Directed,
                                                                  program: PinProgram,
                                                                  step: int) -> None:
    states: list = directed.run(program.image(), program.memmap, steps = program.limit(),
                                extns = program.extns, interrupts = (step,))

    assert landing(states) == step, (
        f"the oracle fired at step {step} and the design took the interrupt at boundary "
        f"{landing(states)}"
    )
    assert handler_entries(states, INTERRUPT_BASE) == 1, "one pin edge is one interrupt"
    assert states[step + 1].csrs["retir"] == states[step].pc, \
        "RETIR names the instruction the interrupt preempted"
    assert states[-1].halted, (
        f"the {program.name} program did not reach a halt in {program.limit()} steps with the "
        f"interrupt at step {step}"
    )


@mark.exhaustive
@mark.parametrize("profile", CYCLE_PROFILES, ids = program_label)
@mark.parametrize("program", PIN_PROGRAMS, ids = program_label)
def test_every_pin_assertion_in_a_window_lands_on_its_boundary(target: Target, spec: SpecDriver,
                                                               program: PinProgram,
                                                               profile: MemoryLatency) -> None:
    directed: Directed = Directed(target, spec, profile)
    image: bytes = program.image()
    cycles: tuple = directed.journal(image, program.memmap, steps = program.limit(0),
                                     extns = program.extns).boundary_cycles

    for step in range(1, program.steps):
        anchor: int = cycles[step - 1]
        offsets: range = range(0, cycles[step] - INT_PIN_LEAD - anchor)
        assert offsets, f"step {step} has no assertion which lands on it, so it tested nothing"

        for after in offsets:
            states: list = directed.run(image, program.memmap, steps = program.limit(),
                                        extns = program.extns, interrupts = (step,),
                                        pin = (IntPulse(step - 1, after),))
            assert landing(states) == step, (
                f"an assertion {after} cycles after boundary {step - 1} landed on boundary "
                f"{landing(states)} rather than {step}"
            )


def test_a_pin_held_for_the_rest_of_the_run_takes_one_interrupt(directed: Directed) -> None:
    program: PinProgram = CALIBRATION
    step: int = PIN_ANCHOR + 1
    held: tuple = (IntPulse(PIN_ANCHOR, 0, HELD_CYCLES),)

    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,),
                                pin = held)
    result: RunResult = directed.journal(program.image(), steps = program.limit(), pin = held)
    rise, fall = result.pin.windows[0]

    assert landing(states) == step
    assert handler_entries(states, INTERRUPT_BASE) == 1, (
        "an interrupt is taken on the edge of the pin and not on its level, so holding it active "
        "over several instructions must take exactly one"
    )
    assert fall > result.cycles, "the pin was still active when the run ended"
    assert sum(1 for cycle in result.boundary_cycles if (rise <= cycle)) > 1, \
        "and it was held across more than one instruction boundary"


def test_two_pin_edges_in_one_run_take_two_interrupts(directed: Directed) -> None:
    program: PinProgram = CALIBRATION
    first: int = PIN_ANCHOR
    second: int = first + len(program.handler) + 2

    states: list = directed.run(program.image(), steps = program.limit(taken = 2),
                                interrupts = (first, second))

    assert handler_entries(states, INTERRUPT_BASE) == 2
    assert states[first + 1].pc == INTERRUPT_BASE
    assert states[second + 1].pc == INTERRUPT_BASE
    assert states[-1].halted


def test_a_pin_edge_during_an_interrupt_entry_is_discarded(directed: Directed) -> None:
    program: PinProgram = STRAIGHT
    step: int = 3
    states: list = directed.run(program.image(), steps = program.limit(),
                                interrupts = (step, step + 1))

    assert handler_entries(states, INTERRUPT_BASE) == 1, (
        "the second edge arrives during the vector read of the first entry, where the extension "
        "is enabled and the interrupt context is not set yet, so int_pending latches -- and is "
        "then thrown away at the boundary the handler starts on, because that boundary is inside "
        "the interrupt context"
    )
    assert states[step + 1].pc == INTERRUPT_BASE
    assert states[-1].halted


def test_a_pin_edge_during_another_handlers_entry_is_taken(directed: Directed) -> None:
    program: PinProgram = SYSCALL
    step: int = 2
    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,))
    entry: MisaState = states[step + 1]

    assert entry.pc == INTERRUPT_BASE, (
        "the edge arrives during the system call's vector read, and a system call context does "
        "not mask interrupts, so the interrupt is taken at the boundary the handler starts on"
    )
    assert entry.in_syscall and entry.in_interrupt, "the two contexts nest"
    assert entry.csrs["retir"] == SYSCALL_BASE, \
        "RETIR names the first instruction of the system call handler"

    returned: MisaState = states[step + 1 + len(program.handler)]
    assert returned.in_syscall and not returned.in_interrupt, \
        "leaving the interrupt context leaves the system call context it nested inside"
    assert states[-1].halted


def test_an_interrupt_taken_in_user_mode_returns_to_user_mode(directed: Directed) -> None:
    program: PinProgram = USER
    step: int = len(USER_MODE_WORDS) + 1

    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,))
    preempted: MisaState = states[step]
    entry: MisaState = states[step + 1]
    returned: MisaState = states[step + 1 + len(program.handler)]

    assert preempted.masked_csr("privs") == USER_MODE, "the program is in User Mode by this step"
    assert entry.masked_csr("privs") == 0x0000, \
        "the handler is entered in Machine Mode, like every other handler"
    assert entry.int_privs == USER_MODE, "with the ring bit saved in the interrupt's own slot"
    assert entry.syscall_privs == 0x0000, "and not in the system call's"
    assert returned.masked_csr("privs") == USER_MODE, "the return restores it"
    assert returned.pc == preempted.pc, "and goes back to the instruction it preempted"
    assert states[-1].halted


def test_a_pin_edge_during_a_return_from_an_interrupt_is_lost(directed: Directed) -> None:
    program: PinProgram = STRAIGHT
    step: int = 2
    returning: int = step + len(program.handler)

    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,),
                                pin = (pin_for_step(step), IntPulse(returning, 0)))

    assert states[returning].pc == INTERRUPT_BASE + 2 * (len(program.handler) - 1), \
        "the boundary the second assertion is anchored to is the return instruction"
    assert handler_entries(states, INTERRUPT_BASE) == 1
    assert states[-1].halted


def test_an_injected_pending_interrupt_is_taken_at_the_first_boundary(directed: Directed) -> None:
    program: PinProgram = STRAIGHT
    image: bytes = program.image()
    state: MisaState = directed.seed(image, int_pending = True)

    states: list = directed.resume(state, image, steps = program.limit())

    assert states[0].int_pending, "the injected state is the one which carries it"
    assert landing(states) == 0
    assert states[1].csrs["retir"] == PROGRAM_BASE
    assert states[-1].halted


TOP_WORD: int = 0xFFFF


def test_an_interrupt_takes_precedence_over_a_program_counter_overflow(
        directed: Directed) -> None:
    program: PinProgram = STRAIGHT
    image: bytes = program.image()
    overflowing: MisaState = directed.seed(image, pc = TOP_WORD)

    faulted: list = directed.resume(overflowing, image, steps = program.limit())
    expect_fault(faulted, FaultKind.PC_OVERFLOW, at = TOP_WORD)
    assert landing(faulted) is None, "with no interrupt pending the fetch is what faults"

    states: list = directed.resume(replace(overflowing, int_pending = True), image,
                                   steps = program.limit())
    entry: MisaState = states[1]

    assert landing(states) == 0, "the interrupt is taken instead of the faulting fetch"
    assert entry.csrs["retir"] == TOP_WORD
    assert entry.masked_csr("cause") == 0x0000, "no fault was raised"
    expect_fault(states, FaultKind.PC_OVERFLOW, at = TOP_WORD)
    assert handler_index(states, INTERRUPT_BASE) < handler_index(states, HANDLER_BASE), \
        "the return goes back to 0xffff, and that is where the fault is raised"


@mark.parametrize("step", (1, 4), ids = program_label)
def test_int_01_retir_names_the_preempted_instruction(directed: Directed, step: int) -> None:
    program: PinProgram = STRAIGHT
    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,))
    entry: MisaState = states[step + 1]

    assert entry.csrs["retir"] == addr_of(step), (
        f"RETIR is {entry.csrs['retir']:#06x} where the interrupt preempted the instruction at "
        f"{addr_of(step):#06x}"
    )
    assert entry.csrs["retir"] != addr_of(step - 1), \
        "and not the instruction which completed before it"


def test_int_07_a_pin_edge_inside_an_interrupt_context_is_discarded(directed: Directed) -> None:
    program: PinProgram = STRAIGHT
    step: int = 2
    inside: int = step + 1

    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,),
                                pin = (pin_for_step(step), IntPulse(inside, 0)))

    assert states[inside].pc == INTERRUPT_BASE, "the second assertion is made inside the handler"
    assert handler_entries(states, INTERRUPT_BASE) == 1, (
        "an edge raised while the processor is in an interrupt context is discarded rather than "
        "deferred, so no second interrupt is taken after the return"
    )
    assert states[-1].halted


@mark.parametrize("cycles", (1, HELD_CYCLES), ids = ("pulse", "held"))
def test_int_08_a_pin_edge_with_the_extension_disabled_is_discarded(directed: Directed,
                                                                    cycles: int) -> None:
    program: PinProgram = replace(STRAIGHT, extns = NO_INTERRUPT)
    step: int = 3

    states: list = directed.run(program.image(), steps = program.limit(0),
                                extns = program.extns, interrupts = (step,),
                                pin = (replace(pin_for_step(step), cycles = cycles),))

    assert handler_entries(states, INTERRUPT_BASE) == 0, \
        "with the Interrupt Extension disabled there is nothing for the pin to do"
    assert len(states) - 1 == program.steps, "the program ran to its end untouched"
    assert states[-1].halted


ENABLE_WORDS: tuple = csr_write_words(CsrReg.EXTNS, Extn.DYNAMICHW | Extn.INTERRUPT)


@mark.parametrize("cycles", (1, HELD_CYCLES), ids = ("pulse", "held"))
def test_int_08_an_edge_lost_while_disabled_is_not_revived_by_enabling_the_extension(
        directed: Directed, cycles: int) -> None:
    program: PinProgram = PinProgram(
        "enabling", "a program which turns the Interrupt Extension on after the pin has fired",
        words = (set_inst(GpReg.RA, 0x01),) + ENABLE_WORDS + (
            set_inst(GpReg.RB, 0x02), set_inst(GpReg.RC, 0x03), halt_inst(GpReg.RA)
        ),
        steps = 7,
        extns = Extn.DYNAMICHW
    )

    states: list = directed.run(program.image(), steps = program.limit(0), extns = program.extns,
                                interrupts = (0,),
                                pin = (replace(pin_for_step(0), cycles = cycles),))

    assert states[4].masked_csr("extns") == (Extn.DYNAMICHW | Extn.INTERRUPT), \
        "the program does enable the extension, four instructions in"
    assert handler_entries(states, INTERRUPT_BASE) == 0, (
        "the edge was lost while the extension was disabled and enabling it does not bring the "
        "interrupt back; no interrupt is taken until the pin is next asserted"
    )
    assert states[-1].halted


@mark.parametrize("after", (0, 4, 8), ids = lambda after: f"after{after}")
def test_int_09_a_pin_edge_after_a_halt_does_nothing(directed: Directed, after: int) -> None:
    program: PinProgram = STRAIGHT
    image: bytes = program.image()
    halting: int = program.steps - 1

    result: RunResult = directed.journal(image, steps = program.limit(0),
                                         pin = (IntPulse(halting, after),), linger = HALT_LINGER)
    expected: MisaState = directed.oracle(image, steps = program.limit(0))[-1]

    assert result.states[-1].halted
    assert result.states[-1].int_pending, (
        "the edge did arrive and latched: what the test is about is that a halted processor does "
        "nothing with it"
    )
    assert landing(result.states) is None
    assert INTERRUPT_VECTOR not in result.bfm.addresses_touched(), \
        "a halted processor does not read the interrupt vector"
    assert not expected.diff(result.states[-1], ignore = BOUNDARY_IGNORED), \
        "and the state it halted in is the one it would have halted in with the pin idle"


def test_int_10_an_instruction_which_would_fault_is_preempted_and_faults_after_the_return(
        directed: Directed) -> None:
    program: PinProgram = FAULTING
    step: int = 1

    states: list = directed.run(program.image(), program.memmap, steps = program.limit(),
                                interrupts = (step,))
    entry: MisaState = states[step + 1]

    assert entry.pc == INTERRUPT_BASE
    assert entry.csrs["retir"] == addr_of(step), "RETIR names the instruction which would fault"
    assert entry.masked_csr("cause") == 0x0000 and entry.csrs["fault"] == 0x0000, \
        "the instruction is not executed, so it has not raised its fault yet"

    expect_fault(states, FaultKind.ILLEGAL, at = addr_of(step))
    assert handler_index(states, INTERRUPT_BASE) < handler_index(states, HANDLER_BASE), \
        "the fault is raised after the return from the interrupt, not before it"
    assert states[-1].halted


def test_int_11_an_interrupt_does_not_change_the_cause_register(directed: Directed) -> None:
    program: PinProgram = PinProgram(
        "marked", "a program which leaves a recognisable CAUSE for the interrupt to not disturb",
        words = csr_write_words(CsrReg.CAUSE, CAUSE_MARK) + (
            set_inst(GpReg.RA, 0x01), set_inst(GpReg.RB, 0x02), halt_inst(GpReg.RA)
        ),
        steps = 6
    )
    step: int = 4

    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,))
    entry: MisaState = states[step + 1]
    returned: MisaState = states[step + 1 + len(program.handler)]

    assert states[step].masked_csr("cause") == CAUSE_MARK, "the program wrote it"
    assert entry.masked_csr("cause") == CAUSE_MARK, (
        "taking an interrupt does not touch CAUSE the way a system call or a fault does, so the "
        "handler can read what the interrupted code left there"
    )
    assert entry.csrs["fault"] == 0x0000, "and it is not a fault, so FAULT is not written either"
    assert returned.masked_csr("cause") == CAUSE_MARK, "nor does the return disturb it"
    assert states[-1].halted


VECTOR_READS: tuple = (
    (None, "w", FaultKind.UNMAPPED_ADDRESS, "unmapped-low"),
    ("w", None, FaultKind.INVALID_ACCESS, "unreadable-low"),
    ("rw", None, FaultKind.UNMAPPED_ADDRESS, "unmapped-high")
)


@mark.parametrize("low, high, kind, name", VECTOR_READS, ids = program_label)
def test_vec_05_an_unreadable_interrupt_vector_discards_the_entry(directed: Directed, low: str,
                                                                  high: str, kind: int,
                                                                  name: str) -> None:
    program: PinProgram = SCRATCHED
    memmap: MemMap = memmap_with((
        (INTERRUPT_VECTOR, INTERRUPT_VECTOR, low),
        (INTERRUPT_VECTOR + 1, INTERRUPT_VECTOR + 1, high)
    ))
    step: int = 3

    states: list = directed.run(program.image(), memmap, steps = program.limit(),
                                interrupts = (step,))
    preempted: MisaState = states[step]
    entry: MisaState = expect_fault(states, kind, AccessKind.READ, at = INTERRUPT_VECTOR)

    assert preempted.int_rscratch == 0x0000, "nothing has been saved before the interrupt"
    assert (preempted.gp_regs[GpReg.RSCRATCH0] == SCRATCH_HIGH) \
        and (preempted.gp_regs[GpReg.RSCRATCH1] == SCRATCH_LOW) \
        and (preempted.masked_csr("flags") == SCRATCH_FLAGS), \
        "and the program has put something in the registers the entry would have saved"

    assert handler_entries(states, INTERRUPT_BASE) == 0, "the interrupt is discarded"
    assert entry.csrs["retir"] == 0x0000, \
        "the entry is discarded along with the rest of the event that raised the fault"
    assert not entry.in_interrupt, "the discarded entry must not leave an interrupt context"
    assert (entry.int_rscratch == 0x0000) and (entry.int_flags == 0x0000), \
        "and the saved RSCRATCH and FLAGS are never written"
    assert (entry.gp_regs[GpReg.RSCRATCH0] == SCRATCH_HIGH) \
        and (entry.gp_regs[GpReg.RSCRATCH1] == SCRATCH_LOW), \
        "nor are the live registers disturbed"
    assert entry.csrs["fault"] == INTERRUPT_VECTOR, \
        "the lower address of the vector is the one reported, whichever byte failed"


def test_rst_04_pin_activity_before_reset_is_discarded(directed: Directed) -> None:
    program: PinProgram = CALIBRATION

    states: list = directed.run(program.image(), steps = program.limit(0),
                                pin = (IntPulse(None, 0, HELD_CYCLES),))

    assert landing(states) is None, (
        "a pin which is already active when the processor leaves reset produces no edge, because "
        "the synchroniser initialises to the active level; any activity before reset is discarded"
    )
    assert len(states) - 1 == program.steps, "the program ran to its end untouched"
    assert states[-1].halted


def test_rst_04_an_interrupt_is_taken_when_the_pin_is_next_asserted(directed: Directed) -> None:
    program: PinProgram = CALIBRATION
    step: int = PIN_ANCHOR + 1
    released: tuple = (IntPulse(None, 0, RESET_HOLD), pin_for_step(step))

    states: list = directed.run(program.image(), steps = program.limit(), interrupts = (step,),
                                pin = released)
    result: RunResult = directed.journal(program.image(), steps = program.limit(), pin = released)
    held, asserted = result.pin.windows

    assert held[1] < asserted[0], (
        "the pin falls after reset and is asserted again later, which is the edge the manual "
        "requires an interrupt to be taken on"
    )
    assert landing(states) == step
    assert handler_entries(states, INTERRUPT_BASE) == 1
    assert states[-1].halted


INTTEST: str = "inttest"
INTTEST_LIMIT: int = 1000
INTTEST_EXTNS: int = Extn.INTERRUPT

INTTEST_EARLY: tuple = (0, 1, 2)
INTTEST_ERASED: tuple = (3, 4, 5)
INTTEST_SEEN: tuple = (6, 7, 20, 40)

INTTEST_WINDOWS: tuple = (
    tuple((step, None) for step in INTTEST_EARLY)
    + tuple((step, "INTTEST_NO_INTERRUPT") for step in INTTEST_ERASED)
    + tuple((step, "INTTEST_SUCCESS") for step in INTTEST_SEEN)
)


def inttest_handler() -> int:
    image: bytes = bringup_image(INTTEST)
    return image[INTERRUPT_VECTOR] | (image[INTERRUPT_VECTOR + 1] << 8)


@mark.parametrize("step, expected", INTTEST_WINDOWS, ids = program_label)
def test_inttest_reports_what_the_pin_gave_it(directed: Directed, bringup_memmap: MemMap,
                                              codes: dict, step: int, expected: str) -> None:
    code: int = (codes[FAULT_TAG] | FaultKind.ADDR_OVERFLOW) if (expected is None) \
        else codes[expected]
    result: RunResult = directed.journal(bringup_image(INTTEST), bringup_memmap,
                                         steps = INTTEST_LIMIT, extns = INTTEST_EXTNS,
                                         pin = pin_schedule((step,)))

    expect_finish(INTTEST, code, result.states, codes)


@mark.parametrize("step", INTTEST_SEEN, ids = program_label)
def test_inttest_runs_in_lockstep_with_the_pin_driven(directed: Directed, bringup_memmap: MemMap,
                                                      step: int) -> None:
    handler: int = inttest_handler()
    states: list = directed.run(bringup_image(INTTEST), bringup_memmap, steps = INTTEST_LIMIT,
                                extns = INTTEST_EXTNS, interrupts = (step,))

    assert landing(states, handler) == step
    assert handler_entries(states, handler) == 1
    assert states[-1].halted
