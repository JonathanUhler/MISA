import pytest

from misa_hw.grammar import Extn
from misa_hw.init import RegInit, CsrInit

import harness
from testlib import spec_exe


def _put16(mem: bytearray, addr: int, word: int) -> None:
    mem[addr] = word & 0xFF
    mem[addr + 1] = (word >> 8) & 0xFF


def test_diff_masks_undefined_flag_bits() -> None:
    a: harness.CosimState = harness.CosimState(special = CsrInit(flags = 0xFFFA))
    b: harness.CosimState = harness.CosimState(special = CsrInit(flags = 0x000A))
    assert harness.diff_states(a, b) == []


def test_diff_detects_flag_nibble_difference() -> None:
    a: harness.CosimState = harness.CosimState(special = CsrInit(flags = 0x5))
    b: harness.CosimState = harness.CosimState(special = CsrInit(flags = 0x6))
    assert harness.diff_states(a, b) != []


def test_diff_ignores_gated_csr_when_extension_off() -> None:
    a: harness.CosimState = harness.CosimState(special = CsrInit(extns = 0x00, retir = 0x3A4F))
    b: harness.CosimState = harness.CosimState(special = CsrInit(extns = 0x00, retir = 0x0000))
    assert harness.diff_states(a, b) == []


def test_diff_reports_gated_csr_when_extension_on() -> None:
    on: int = Extn.DYNAMICHW | Extn.INTERRUPT
    a: harness.CosimState = harness.CosimState(special = CsrInit(extns = on, retir = 0x3A4F))
    b: harness.CosimState = harness.CosimState(special = CsrInit(extns = on, retir = 0x0000))
    assert harness.diff_states(a, b) != []


def test_diff_ignores_context_flag_when_extension_off() -> None:
    a: harness.CosimState = harness.CosimState(special = CsrInit(extns = 0x00), in_interrupt = True)
    b: harness.CosimState = harness.CosimState(special = CsrInit(extns = 0x00), in_interrupt = False)
    assert harness.diff_states(a, b) == []


def test_run_hw_trace_retire_count() -> None:
    mem: bytearray = bytearray(65536)
    _put16(mem, 0xC000, 0x0119)   # SET RA 1
    _put16(mem, 0xC002, 0x0229)   # SET RB 2
    _put16(mem, 0xC004, 0x0339)   # SET RC 3
    st: harness.CosimState = harness.CosimState(
        special = CsrInit(extns = 0x00), pc = 0xC000, memory = bytes(mem))
    trace: list = harness.run_hw_trace(st, 3)
    assert len(trace) == 3
    assert [s.pc for s in trace] == [0xC002, 0xC004, 0xC006]
    assert trace[0].registers.ra == 1
    assert trace[2].registers.rc == 3


def test_lockstep_rejects_short_trace_without_halt() -> None:
    mem: bytearray = bytearray(65536)
    _put16(mem, 0xC000, 0x0119)   # SET RA 1
    _put16(mem, 0xC002, 0x0229)   # SET RB 2
    _put16(mem, 0xC004, 0x0339)   # SET RC 3
    st: harness.CosimState = harness.CosimState(
        special = CsrInit(extns = 0x00), pc = 0xC000, memory = bytes(mem))
    with pytest.raises(AssertionError):
        harness.lockstep(st, n = 20, max_cycles = 5)


@pytest.mark.skipif(spec_exe() is None, reason="misa-spec-exe oracle not found")
def test_rsr_of_extns_upper_bits_does_not_launder() -> None:
    mem: bytearray = bytearray(65536)
    _put16(mem, 0xC000, 0x0319)   # SET RA 0x03
    _put16(mem, 0xC002, 0x0129)   # SET RB 0x01
    _put16(mem, 0xC004, 0x521D)   # WSR EXTNS RA RB   (writes 0x0301)
    _put16(mem, 0xC006, 0x543C)   # RSR EXTNS RC RD
    _put16(mem, 0xC008, 0x0000)   # HALT R0
    st: harness.CosimState = harness.CosimState(
        special = CsrInit(extns = Extn.DYNAMICHW), pc = 0xC000, memory = bytes(mem))
    harness.lockstep(st, 8)


@pytest.mark.skipif(spec_exe() is None, reason="misa-spec-exe oracle not found")
def test_directed_add_lockstep() -> None:
    mem: bytearray = bytearray(65536)
    _put16(mem, 0xC000, 0x2131)   # ADD RC RA RB
    st: harness.CosimState = harness.CosimState(
        registers = RegInit(ra = 5, rb = 3), special = CsrInit(extns = 0x00),
        pc = 0xC000, memory = bytes(mem))
    harness.lockstep(st, 1)       # raises on any divergence
