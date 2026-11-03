"""Interrupt extension: edge-triggered entry, context save/restore, masking, and the syscall fault.

The irq pin is pulsed via run_until_halt's irq_schedule. The interrupt handler lives at 0xFFF8/9.
"""

from conftest import make_image, put16, run_until_halt

IRQ: int = 0x09       # dynamichw + interrupt
IRQ_SYS: int = 0x0F   # all extensions (for the syscall-in-interrupt fault)


def _base(main: list, handler: list, handler_at: int = 0xC030) -> list:
    img: list = make_image()
    put16(img, 0xFFFE, 0xC000)
    put16(img, 0xFFF8, handler_at)
    addr: int = 0xC000
    for w in main:
        put16(img, addr, w)
        addr += 2
    addr = handler_at
    for w in handler:
        put16(img, addr, w)
        addr += 2
    return img


def test_interrupt_taken_runs_handler_and_returns() -> None:
    # RA is written only by the handler; reaching HALT RA with 0x5A proves the interrupt ran.
    img: list = _base(
        main=[0x0029, 0x0039, 0x0049, 0x0010],   # SET RB/RC/RD 0 ; HALT RA
        handler=[0x5A19, 0x809F],                 # SET RA 0x5A ; RETI
    )
    r: dict = run_until_halt(img, extns_reset=IRQ, irq_schedule={5: 1})
    assert r["halt_code"] == 0x5A


def test_interrupt_disabled_ignores_irq() -> None:
    img: list = _base(
        main=[0x0029, 0x0039, 0x0049, 0x0010],   # HALT RA (RA stays 0)
        handler=[0x5A19, 0x809F],
    )
    r: dict = run_until_halt(img, extns_reset=0x00, irq_schedule={5: 1})
    assert r["halt_code"] == 0x00


def test_interrupt_restores_rscratch() -> None:
    # Main spins until the handler sets a ran-flag (RB), then HALTs RSCRATCH0. The handler clobbers
    # RSCRATCH0 to 0xBB; RETI must restore it to 0xAA. This distinguishes all three outcomes:
    #   restore works -> 0xAA; restore broken -> 0xBB; interrupt never fires -> spins to timeout.
    img: list = _base(
        main=[
            0xAAE9,  # C000 SET RSCRATCH0 0xAA  (value to preserve)
            0x0029,  # C002 SET RB 0x00         (ran-flag)
            0xC059,  # C004 SET RE 0xC0         (loop target hi)
            0x0869,  # C006 SET RF 0x08         (loop target lo = 0xC008)
            0x0203,  # C008 SUB R0 RB R0        (Z = RB == 0)
            0x165F,  # C00A JMP EQUAL RE RF     (spin while RB == 0)
            0x00E0,  # C00C HALT RSCRATCH0
        ],
        handler=[0xBBE9, 0x0129, 0x809F],        # SET RSCRATCH0 0xBB ; SET RB 0x01 ; RETI
    )
    # Fire once the program is spinning (well past main's RB init), so the handler's ran-flag sticks.
    r: dict = run_until_halt(img, extns_reset=IRQ, irq_schedule={40: 1})
    assert r["halt_code"] == 0xAA


def test_syscall_in_interrupt_context_faults() -> None:
    img: list = _base(
        main=[0x0029, 0x0039, 0x0049, 0x0010],   # HALT RA
        handler=[0x8010],                         # SYSCALL RA  (inside interrupt context) -> fault
    )
    put16(img, 0xFFFC, 0xC050)   # fault vector
    put16(img, 0xC050, 0x421C)   # RSR CAUSE RA RB
    put16(img, 0xC052, 0x0020)   # HALT RB  -> CAUSE low byte 0x13
    r: dict = run_until_halt(img, extns_reset=IRQ_SYS, irq_schedule={5: 1})
    assert r["halt_code"] == 0x13
