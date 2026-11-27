from dataclasses import dataclass, replace

from misa_hw.grammar import FaultKind
from misa_hw.init import CoreInit

from generators import DEFAULT_EXTNS, HANDLER_BASE
from memory import MemMap, MemoryLatency
from programs import PROGRAM_BASE, cause_kind, cause_reason, cause_status, cause_type, cause_word
from spec import SpecDriver
from state import BOUNDARY_IGNORED, MisaState
from target import RunResult, Target, pin_schedule


EVERYTHING: MemMap = MemMap.everything()


class DivergenceError(AssertionError):
    ...


def format_divergence(index: int, executed_at: MisaState, oracle: MisaState,
                      design: MisaState, diffs: list) -> str:
    lines: list = [
        f"divergence at state index {index}",
        f"  caused by the instruction at pc={executed_at.pc:#06x}"
        if (executed_at is not None) else
        "  at the initial state, before any instruction executed",
        f"  oracle: {oracle.summary()}",
        f"  design: {design.summary()}",
        "  fields (oracle -> design):"
    ]
    for name, oracle_value, design_value in diffs:
        if (isinstance(oracle_value, int)) and (isinstance(design_value, int)):
            lines.append(f"    {name}: {oracle_value:#x} -> {design_value:#x}")
        else:
            lines.append(f"    {name}: {oracle_value} -> {design_value}")
    return "\n".join(lines)


def check_lengths(oracle_states: list, design: RunResult, steps: int) -> None:
    expected: int = len(oracle_states)
    produced: int = len(design.states)
    if (expected == produced):
        return

    oracle_halted: bool = oracle_states[-1].halted
    raise DivergenceError(
        f"the design produced {produced} states and the oracle {expected}"
        f" (requested steps={steps}; oracle halted={oracle_halted},"
        f" design halted={design.halted}, design ran {design.cycles} cycles).\n"
        f"  A short design trace means the core stalled or a boundary was missed, not that the\n"
        f"  comparison may be truncated to the shorter of the two."
    )


def check_count(design: RunResult, states: int, steps: int) -> None:
    if (design.observed() == states):
        return
    raise DivergenceError(
        f"the design passed through {design.observed()} states where the oracle passed through "
        f"{states} (requested steps={steps}; design halted={design.halted} after "
        f"{design.boundaries} boundaries and {design.cycles} cycles).\n"
        f"  A design which retired fewer instructions than the oracle did is a stalled core or a\n"
        f"  missed boundary, and its final state matching proves nothing."
    )


def steps_from(label: str, entry: MisaState, expected: MisaState, design: RunResult,
               steps: int = 1, ignore: tuple = BOUNDARY_IGNORED) -> None:
    owed: int = steps + 1
    if (len(design.states) != owed) and (not (expected.halted and design.halted)):
        raise DivergenceError(
            f"{label}: the design produced {len(design.states)} states for {steps} "
            f"instruction{'' if (steps == 1) else 's'}, where it owes the boundary before them "
            f"and the state after each (it ran {design.cycles} cycles and "
            f"{'halted' if (design.halted) else 'did not halt'})"
        )

    injected: list = entry.diff(design.states[0], ignore = ignore + ("memory",))
    if (injected):
        raise DivergenceError(
            f"{label}: the design did not start from the injected seed state\n"
            + format_divergence(0, None, entry, design.states[0], injected)
        )

    diffs: list = expected.diff(design.states[-1], ignore = ignore)
    if (diffs):
        raise DivergenceError(
            f"{label}\n"
            + format_divergence(len(design.states) - 1, entry, expected, design.states[-1], diffs)
        )


def single_step(label: str, entry: MisaState, expected: MisaState, design: RunResult,
                ignore: tuple = BOUNDARY_IGNORED) -> None:
    steps_from(label, entry, expected, design, steps = 1, ignore = ignore)


def check_reset_arguments(init: CoreInit, state: MisaState, extns: int) -> None:
    if (state is not None) and (init is not None):
        raise ValueError("pass either a MisaState to resume from or a CoreInit, not both")
    if (state is None) and (extns is None):
        raise ValueError("a run from reset needs an explicit extns, the reset value of EXTNS")
    if (state is not None) and (extns is not None):
        raise ValueError("a resumed run takes EXTNS from the state; do not also pass extns")


def interrupt_arguments(interrupts: tuple, pin: tuple) -> tuple:
    if (pin is None):
        return pin_schedule(interrupts)
    return pin


def lockstep(target: Target, spec: SpecDriver, image: bytes, memmap: MemMap,
             init: CoreInit = None, state: MisaState = None, steps: int = None,
             extns: int = None, latency: MemoryLatency = None,
             ignore: tuple = BOUNDARY_IGNORED, interrupts: tuple = (),
             pin: tuple = None) -> list:
    check_reset_arguments(init, state, extns)
    driven: tuple = interrupt_arguments(interrupts, pin)

    if (state is not None):
        oracle_states: list = spec.qi(state, memmap, steps = steps, interrupts = interrupts,
                                      trace = True)
        result: RunResult = target.run(
            image, state.to_core_init(), steps, driven, memmap, latency
        )
    else:
        oracle_states = spec.q0(image, memmap, extns, steps = steps, interrupts = interrupts,
                                trace = True)
        result = target.run(
            image, init, steps, driven, memmap, latency, reset_extns = extns
        )

    check_lengths(oracle_states, result, steps)

    for index, (expected, produced) in enumerate(zip(oracle_states, result.states)):
        diffs: list = expected.diff(produced, ignore = ignore)
        if (diffs):
            raise DivergenceError(format_divergence(
                index, oracle_states[index - 1] if (index > 0) else None,
                expected, produced, diffs
            ))

    return oracle_states


def final(target: Target, spec: SpecDriver, image: bytes, memmap: MemMap,
          init: CoreInit = None, state: MisaState = None, steps: int = None,
          extns: int = None, latency: MemoryLatency = None,
          ignore: tuple = BOUNDARY_IGNORED, interrupts: tuple = (),
          pin: tuple = None) -> MisaState:
    check_reset_arguments(init, state, extns)
    driven: tuple = interrupt_arguments(interrupts, pin)

    if (state is not None):
        oracle_final: MisaState = spec.qi(state, memmap, steps = steps,
                                          interrupts = interrupts)[-1]
        result: RunResult = target.run(
            image, state.to_core_init(), steps, driven, memmap, latency, record = False
        )
    else:
        oracle_final = spec.q0(image, memmap, extns, steps = steps, interrupts = interrupts)[-1]
        result = target.run(image, init, steps, driven, memmap, latency, reset_extns = extns,
                            record = False)

    if (not result.states):
        raise DivergenceError(
            f"the design produced no state at all after {result.cycles} cycles and "
            f"{result.boundaries} boundaries, where the oracle ended "
            f"{'halted' if (oracle_final.halted) else 'running'} at pc={oracle_final.pc:#06x}"
        )

    design_final: MisaState = result.states[-1]
    if (oracle_final.halted != design_final.halted):
        raise DivergenceError(
            f"the oracle halted={oracle_final.halted} and the design "
            f"halted={design_final.halted} after steps={steps} "
            f"({result.boundaries} design boundaries, {result.cycles} cycles)"
        )
    if (not oracle_final.halted) and (steps is not None):
        check_count(result, steps + 1, steps)

    diffs: list = oracle_final.diff(design_final, ignore = ignore)
    if (diffs):
        raise DivergenceError(format_divergence(
            result.observed() - 1, None, oracle_final, design_final, diffs
        ))

    return oracle_final


def compared(mode: str, target: Target, spec: SpecDriver, image: bytes, memmap: MemMap,
             init: CoreInit = None, state: MisaState = None, steps: int = None,
             extns: int = None, latency: MemoryLatency = None,
             ignore: tuple = BOUNDARY_IGNORED, interrupts: tuple = (),
             pin: tuple = None) -> list:
    if (mode not in COMPARISONS):
        raise ValueError(f"'{mode}' is not a comparison mode; use one of {sorted(COMPARISONS)}")
    return COMPARISONS[mode](target, spec, image, memmap, init = init, state = state,
                             steps = steps, extns = extns, latency = latency, ignore = ignore,
                             interrupts = interrupts, pin = pin)


COMPARISONS: dict = {
    "lockstep": lockstep,
    "final": lambda *args, **changes: [final(*args, **changes)]
}


def state_at(states: list, pc: int) -> MisaState:
    for state in states:
        if (state.pc == pc):
            return state
    raise AssertionError(
        f"no state of the run sat at pc={pc:#06x}; the run visited "
        f"{', '.join(f'{state.pc:#06x}' for state in states)}"
    )


def handler_index(states: list, handler: int = HANDLER_BASE) -> int:
    for index, state in enumerate(states):
        if (state.pc == handler):
            return index
    raise AssertionError(
        f"the run never entered the handler at {handler:#06x}; it ended "
        f"{'halted' if (states[-1].halted) else 'running'} at pc={states[-1].pc:#06x} with "
        f"cause={states[-1].masked_csr('cause'):#06x} fault={states[-1].csrs['fault']:#06x}"
    )


def handler_entries(states: list, handler: int = HANDLER_BASE) -> int:
    return sum(1 for state in states if (state.pc == handler))


def describe_cause(cause: int) -> str:
    kind: int = cause_kind(cause)
    name: str = FaultKind(kind).name if (kind in tuple(FaultKind)) else "unassigned"
    return f"{cause:#06x} (reason {cause_reason(cause):#05b} type {cause_type(cause):#05b} " \
           f"{name}, extended status {cause_status(cause):#04x})"


def expect_fault(states: list, kind: int, status: int = 0x00, at: int = PROGRAM_BASE,
                 handler: int = HANDLER_BASE) -> MisaState:
    entry: MisaState = states[handler_index(states, handler)]
    expected: int = cause_word(kind, status)

    assert entry.masked_csr("cause") == expected, (
        f"the handler was entered with CAUSE {describe_cause(entry.masked_csr('cause'))} where "
        f"the manual requires {describe_cause(expected)}"
    )
    assert entry.csrs["fault"] == at, (
        f"FAULT is {entry.csrs['fault']:#06x} where the manual requires {at:#06x}, the address "
        f"the fault was detected at"
    )
    return entry


def expect_no_fault(states: list) -> MisaState:
    for state in states:
        assert state.pc != HANDLER_BASE, (
            f"the run entered the fault handler with CAUSE "
            f"{describe_cause(state.masked_csr('cause'))} and FAULT {state.csrs['fault']:#06x}"
        )
    return states[-1]


@dataclass(frozen = True)
class Directed:

    target: Target
    spec: SpecDriver
    latency: MemoryLatency


    def run(self, image: bytes, memmap: MemMap = EVERYTHING, steps: int = None,
            extns: int = DEFAULT_EXTNS, interrupts: tuple = (), pin: tuple = None) -> list:
        return lockstep(self.target, self.spec, image, memmap, steps = steps, extns = extns,
                        latency = self.latency, interrupts = interrupts, pin = pin)


    def resume(self, state: MisaState, image: bytes, memmap: MemMap = EVERYTHING,
               steps: int = None, interrupts: tuple = (), pin: tuple = None) -> list:
        return lockstep(self.target, self.spec, image, memmap, state = state, steps = steps,
                        latency = self.latency, interrupts = interrupts, pin = pin)


    def journal(self, image: bytes, memmap: MemMap = EVERYTHING, steps: int = None,
                extns: int = DEFAULT_EXTNS, pin: tuple = (), linger: int = 0) -> RunResult:
        return self.target.run(image, None, steps, pin, memmap, self.latency,
                               reset_extns = extns, linger = linger)


    def oracle(self, image: bytes, memmap: MemMap = EVERYTHING, steps: int = None,
               extns: int = DEFAULT_EXTNS, interrupts: tuple = ()) -> list:
        return self.spec.q0(image, memmap, extns, steps = steps, interrupts = interrupts,
                            trace = True)


    def seed(self, image: bytes, memmap: MemMap = EVERYTHING, extns: int = DEFAULT_EXTNS,
             csrs: dict = None, regs: dict = None, **changes: object) -> MisaState:
        state: MisaState = MisaState.from_spec_text(
            self.spec.state_file_text(image, memmap, extns)
        )[0]

        if (csrs):
            changes["csrs"] = {**state.csrs, **csrs}
        if (regs):
            values: list = list(state.gp_regs)
            for slot, value in regs.items():
                values[slot] = value
            changes["gp_regs"] = tuple(values)

        return replace(state, **changes)
