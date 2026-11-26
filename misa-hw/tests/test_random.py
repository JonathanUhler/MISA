from pytest import MarkDecorator, mark

from compare import compared
from generators import (
    CAMPAIGN_MEMMAP, CAMPAIGN_SEEDS, CAMPAIGN_VECTORS, DEFAULT_EXTNS, HANDLER_BASE, PAGE_COUNT,
    RANDOM_CURSOR, RANDOM_WALKER, random_image, random_memmap
)
from memory import CAPS_RWX, MEMORY_SIZE, MemMap, MemoryLatency
from programs import PROGRAM_BASE, RESET_VECTOR, random_memory, word_bytes
from spec import SpecDriver
from state import MisaState
from target import Target


pytestmark: MarkDecorator = mark.random

RANDOM_STEPS: int = 200
RETIRED_ENOUGH: int = 10
QUICK_SEEDS: int = CAMPAIGN_SEEDS[0]


def random_map(seed: int) -> MemMap:
    return CAMPAIGN_MEMMAP if (seed % 2 == 0) else random_memmap(seed)


def test_a_random_image_is_the_same_image_every_time() -> None:
    assert random_image(7) == random_image(7), "a seed has to name one image forever"
    assert random_image(7) != random_image(8), "and different seeds different images"
    assert len(random_image(7)) == MEMORY_SIZE


def test_a_random_image_carries_a_valid_vector_table_and_a_walker() -> None:
    image: bytes = random_image(3)
    for addr, target in CAMPAIGN_VECTORS.items():
        assert image[addr] | (image[addr + 1] << 8) == target, (
            f"the vector at {addr:#06x} is not the handler the campaign placed there; without "
            f"one every run would be the reset-fault path over again"
        )

    walker: bytes = b"".join(word_bytes(word) for word in RANDOM_WALKER)
    assert image[HANDLER_BASE:HANDLER_BASE + len(walker)] == walker, \
        "the fault handler is the walker, not whatever the fill left at that address"
    assert image[:HANDLER_BASE] == random_memory(3)[:HANDLER_BASE], \
        "everything below the handlers is the random fill, untouched"


def test_a_random_memory_map_grants_what_the_run_needs() -> None:
    memmap: MemMap = random_memmap(5)
    for addr in (RESET_VECTOR, PROGRAM_BASE, HANDLER_BASE, RANDOM_CURSOR):
        assert memmap.caps_at(addr) == CAPS_RWX, (
            f"{addr:#06x} has to be reachable or the campaign only ever tests the vector path"
        )

    granted: int = sum(1 for page in range(PAGE_COUNT)
                       if (memmap.caps_at(page * 0x100) == CAPS_RWX))
    assert granted < PAGE_COUNT, "a random map which grants everything is not a random map"
    assert memmap == random_memmap(5), "a seed has to name one map forever"


def test_the_campaign_reaches_past_the_first_instruction(spec: SpecDriver) -> None:
    retired: list = []
    causes: set = set()
    for seed in range(QUICK_SEEDS):
        states: list = spec.q0(random_image(seed), random_map(seed), DEFAULT_EXTNS,
                               steps = RANDOM_STEPS, trace = True)
        retired.append(len(states) - 1)
        causes |= {state.masked_csr("cause") for state in states}

    assert min(retired) >= 1, "an image which retires nothing tests only the reset vector"
    assert sum(retired) / len(retired) >= RETIRED_ENOUGH, (
        f"the quick seeds retire {sum(retired) / len(retired):.1f} instructions on average, "
        f"where the campaign is meant to run programs rather than fault out of them"
    )
    assert len(causes) >= 4, f"the campaign produced only the causes {sorted(causes)}"


@mark.slow
def test_a_random_image_runs_the_same_way(target: Target, spec: SpecDriver,
                                          latency: MemoryLatency, compare_mode: str,
                                          campaign_seed: int) -> None:
    states: list = compared(compare_mode, target, spec, random_image(campaign_seed),
                            random_map(campaign_seed), steps = RANDOM_STEPS,
                            extns = DEFAULT_EXTNS, latency = latency)
    ended: MisaState = states[-1]

    assert ended.halted or (compare_mode != "lockstep") or (len(states) == RANDOM_STEPS + 1), (
        f"seed {campaign_seed} compared {len(states) - 1} instructions of the {RANDOM_STEPS} it "
        f"was given, and did not halt"
    )
