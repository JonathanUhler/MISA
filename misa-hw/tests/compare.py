from memory import MemMap, MemoryLatency
from misa_hw.init import CoreInit
from spec import SpecDriver
from state import BOUNDARY_IGNORED, MisaState
from target import RunResult, Target


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
    design_halted: bool = design.states[-1].halted if (design.states) else False
    raise DivergenceError(
        f"the design produced {produced} states and the oracle {expected}"
        f" (requested steps={steps}; oracle halted={oracle_halted},"
        f" design halted={design_halted}, design ran {design.cycles} cycles).\n"
        f"  A short design trace means the core stalled or a boundary was missed, not that the\n"
        f"  comparison may be truncated to the shorter of the two."
    )


def single_step(label: str, entry: MisaState, expected: MisaState, design: RunResult,
                ignore: tuple = BOUNDARY_IGNORED) -> None:
    if (len(design.states) != 2):
        halted: bool = bool(design.states) and design.states[-1].halted
        raise DivergenceError(
            f"{label}: the design produced {len(design.states)} states for one instruction, "
            f"where it owes the boundary before it and the state after it (it ran "
            f"{design.cycles} cycles and {'halted' if (halted) else 'did not halt'})"
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
            f"{label}\n" + format_divergence(1, entry, expected, design.states[-1], diffs)
        )


def check_reset_arguments(init: CoreInit, state: MisaState, extns: int) -> None:
    if (state is not None) and (init is not None):
        raise ValueError("pass either a MisaState to resume from or a CoreInit, not both")
    if (state is None) and (extns is None):
        raise ValueError("a run from reset needs an explicit extns, the reset value of EXTNS")
    if (state is not None) and (extns is not None):
        raise ValueError("a resumed run takes EXTNS from the state; do not also pass extns")


def lockstep(target: Target, spec: SpecDriver, image: bytes, memmap: MemMap,
             init: CoreInit = None, state: MisaState = None, steps: int = None,
             extns: int = None, latency: MemoryLatency = None,
             ignore: tuple = BOUNDARY_IGNORED) -> list:
    check_reset_arguments(init, state, extns)

    if (state is not None):
        oracle_states: list = spec.qi(state, memmap, steps = steps, trace = True)
        result: RunResult = target.run(
            image, state.to_core_init(), steps, (), memmap, latency
        )
    else:
        oracle_states = spec.q0(image, memmap, extns, steps = steps, trace = True)
        result = target.run(
            image, init, steps, (), memmap, latency, reset_extns = extns
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
          ignore: tuple = BOUNDARY_IGNORED) -> MisaState:
    check_reset_arguments(init, state, extns)

    if (state is not None):
        oracle_final: MisaState = spec.qi(state, memmap, steps = steps)[-1]
        result: RunResult = target.run(
            image, state.to_core_init(), steps, (), memmap, latency
        )
    else:
        oracle_final = spec.q0(image, memmap, extns, steps = steps)[-1]
        result = target.run(image, init, steps, (), memmap, latency, reset_extns = extns)

    design_final: MisaState = result.states[-1]
    if (oracle_final.halted != design_final.halted):
        raise DivergenceError(
            f"the oracle halted={oracle_final.halted} and the design "
            f"halted={design_final.halted} after steps={steps} "
            f"({len(result.states)} design states, {result.cycles} cycles)"
        )

    diffs: list = oracle_final.diff(design_final, ignore = ignore)
    if (diffs):
        raise DivergenceError(format_divergence(
            len(result.states) - 1, None, oracle_final, design_final, diffs
        ))

    return oracle_final
