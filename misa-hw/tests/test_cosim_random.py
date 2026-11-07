import random

import pytest

import generators
import harness
from testlib import spec_exe, toolchain_dir

pytestmark = pytest.mark.skipif(spec_exe() is None, reason = "misa-spec-exe oracle not found")

_STEPS: int = 24
_LONG_STEPS: int = 30


def test_uniform_walk(cosim_seed: int) -> None:
    state: harness.CosimState = generators.uniform_state(random.Random(cosim_seed))
    try:
        harness.lockstep(state, _STEPS)
    except AssertionError as e:
        raise AssertionError(f"[uniform seed={cosim_seed}] {e}") from e


def test_biased_straight_line(cosim_seed: int) -> None:
    state: harness.CosimState = generators.biased_state(random.Random(cosim_seed))
    try:
        harness.lockstep(state, _LONG_STEPS)
    except AssertionError as e:
        raise AssertionError(f"[biased seed={cosim_seed}] {e}") from e


@pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")
def test_callchain(cosim_seed: int) -> None:
    from testlib import assemble
    src: str = generators.callchain_source(random.Random(cosim_seed))
    image: list = assemble(src)
    state: harness.CosimState = harness.reset_state(image, 0)
    try:
        harness.lockstep(state, 60)
    except AssertionError as e:
        raise AssertionError(f"[callchain seed={cosim_seed}]\n{src}\n{e}") from e
