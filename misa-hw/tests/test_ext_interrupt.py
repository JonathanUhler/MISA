"""Interrupt extension: edge-triggered entry, context save/restore, masking, and the syscall fault.

The irq pin is pulsed via run_program's irq_schedule. The interrupt handler lives at 0xFFF8/9.
"""

import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")

IRQ = ("dynamichw", "interrupt")
IRQ_SYS = ("dynamichw", "syscall", "interrupt")


def test_interrupt_taken_runs_handler_and_returns() -> None:
    # RA is written only by the handler; reaching HALT RA with 0x5A proves the interrupt ran.
    r: dict = run_program("""
        set rb 0x00
        set rc 0x00
        set rd 0x00
        halt ra
    _irq:
        set ra 0x5a
        reti
    """, extns=IRQ, interrupt="_irq", irq_schedule={5: 1})
    assert r["halt_code"] == 0x5A


def test_interrupt_disabled_ignores_irq() -> None:
    # Assembled with -e interrupt (so RETI parses) but run with interrupts disabled: RA stays 0.
    r: dict = run_program("""
        set rb 0x00
        set rc 0x00
        set rd 0x00
        halt ra
    _irq:
        set ra 0x5a
        reti
    """, extns=(), as_extns=("interrupt",), interrupt="_irq", irq_schedule={5: 1})
    assert r["halt_code"] == 0x00


def test_interrupt_restores_rscratch() -> None:
    # Main spins until the handler sets a ran-flag (RB), then HALTs RSCRATCH0. The handler clobbers
    # RSCRATCH0 to 0xBB; RETI must restore it to 0xAA. Distinguishes all three outcomes:
    #   restore works -> 0xAA; restore broken -> 0xBB; interrupt never fires -> spins to timeout.
    # The spin loop uses a preloaded register pair (REF) so it never touches RSCRATCH.
    r: dict = run_program("""
        set rscratch0 0xaa
        set rb 0x00
        set2 ref _loop
    _loop:
        sub r0 rb r0
        jmp equal re rf
        halt rscratch0
    _irq:
        set rscratch0 0xbb
        set rb 0x01
        reti
    """, extns=IRQ, interrupt="_irq", irq_schedule={40: 1})
    assert r["halt_code"] == 0xAA


def test_syscall_in_interrupt_context_faults() -> None:
    r: dict = run_program("""
        set rb 0x00
        set rc 0x00
        set rd 0x00
        halt ra
    _irq:
        syscall ra
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=IRQ_SYS, interrupt="_irq", fault="_fault", irq_schedule={5: 1})
    assert r["halt_code"] == 0x13
