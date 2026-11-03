"""Equivalence of the Amaranth core against the misa-spec oracle on single-instruction steps."""

import os
import random
import sys

import pytest

sys.path.insert(0, os.path.dirname(__file__))
from harness import run_hw, step_reference, diff_states, spec_exe, REG_NAMES, CSR_ORDER  # noqa: E402

pytestmark: object = pytest.mark.skipif(
    spec_exe() is None, reason="misa-spec-exe oracle not found on PATH or in install/")


def blank_state(extns: int = 0) -> dict:
    return {
        "halted": False,
        "inSyscall": False,
        "inInterrupt": False,
        "syscallPrivs": 0,
        "intRscratch": 0,
        "intFlags": 0,
        "intPrivs": 0,
        "pc": 0xC000,
        "special": {name: 0 for name in CSR_ORDER} | {"extns": extns},
        "registers": {name: 0 for name in REG_NAMES},
        "memory": bytearray(65536),
    }


def put_inst(state: dict, addr: int, word: int) -> None:
    state["memory"][addr] = word & 0xFF
    state["memory"][addr + 1] = (word >> 8) & 0xFF


def assert_equiv(state: dict, n: int = 1) -> None:
    ref: dict = step_reference(state, n)
    hw: dict = run_hw(state, n)
    diffs: list = diff_states(ref, hw)
    assert not diffs, "HW/oracle divergence:\n  " + "\n  ".join(diffs)


def test_directed_add() -> None:
    st: dict = blank_state()
    st["registers"]["ra"] = 0x05
    st["registers"]["rb"] = 0x03
    put_inst(st, 0xC000, 0x2131)  # ADD RC RA RB
    assert_equiv(st)


def test_directed_sub_sets_flags() -> None:
    st: dict = blank_state()
    st["registers"]["ra"] = 0x00
    st["registers"]["rb"] = 0x01
    put_inst(st, 0xC000, 0x0203)  # SUB R0 RA RB  (0 - 1 -> flags N set, C clear)
    assert_equiv(st)


def test_directed_store() -> None:
    st: dict = blank_state()
    st["registers"]["ra"] = 0x02   # base hi
    st["registers"]["rb"] = 0x10   # base lo -> 0x0210
    st["registers"]["rc"] = 0x5A
    put_inst(st, 0xC000, 0x013B)   # ST RC [RA:RB]+0
    assert_equiv(st)


def test_directed_illegal_faults_identically() -> None:
    st: dict = blank_state()
    put_inst(st, 0xC000, 0x600C)   # illegal RSR (CSR field 0x6)
    assert_equiv(st)


def _random_state(rng: random.Random) -> dict:
    # Plausible extension set: random low nibble, Dynamic Hardware usually on.
    extns: int = rng.randint(0, 0xF)
    st: dict = blank_state(extns=extns)
    st["pc"] = rng.randint(0, 0xFF00) & 0xFFFE
    for name in REG_NAMES:
        if name != "r0":
            st["registers"][name] = rng.randint(0, 0xFF)
    for name in ("saddr", "raddr", "cause"):
        st["special"][name] = rng.randint(0, 0xFFFF)
    st["special"]["flags"] = rng.randint(0, 0xF)
    # Extension-gated CSRs: only meaningful (and only compared) when their extension is on.
    if extns & (1 << 1):
        st["special"]["retsc"] = rng.randint(0, 0xFFFF)
    if extns & (1 << 3):
        st["special"]["retir"] = rng.randint(0, 0xFFFF)
    if extns & (1 << 2):
        st["special"]["privs"] = rng.randint(0, 1)
    # Plausible context bits (only when the relevant extension is enabled, never both at once).
    if (extns & (1 << 1)) and rng.random() < 0.3:
        st["inSyscall"] = True
        st["syscallPrivs"] = rng.randint(0, 1)
    elif (extns & (1 << 3)) and rng.random() < 0.3:
        st["inInterrupt"] = True
        st["intRscratch"] = rng.randint(0, 0xFFFF)
        st["intFlags"] = rng.randint(0, 0xF)
        st["intPrivs"] = rng.randint(0, 1)
    # Fill memory with random bytes so loads/fetches see varied data.
    st["memory"] = bytearray(rng.randint(0, 255) for _ in range(65536))
    # Place a random instruction word at pc.
    put_inst(st, st["pc"], rng.randint(0, 0xFFFF))
    return st


@pytest.mark.parametrize("seed", range(60))
def test_random_single_step_equivalence(seed: int) -> None:
    rng: random.Random = random.Random(seed)
    st: dict = _random_state(rng)
    ref: dict = step_reference(st, 1)
    hw: dict = run_hw(st, 1)
    diffs: list = diff_states(ref, hw)
    assert not diffs, (
        f"divergence (seed={seed}, inst at pc={st['pc']:#06x} = "
        f"{st['memory'][st['pc']] | (st['memory'][st['pc'] + 1] << 8):#06x}, "
        f"extns={st['special']['extns']:#x}):\n  " + "\n  ".join(diffs)
    )
