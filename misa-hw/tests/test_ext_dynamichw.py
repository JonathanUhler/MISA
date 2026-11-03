"""Dynamic Hardware: EXTNS is writable, bit 0 pinned, writes ignored when Dynamic Hardware is off.

Observed through CSR availability: RSR RETSC is legal only while EXTNS.syscall is set, so toggling
the syscall bit via WSR EXTNS changes whether a following RSR RETSC faults as illegal (CAUSE 0x09).
"""

from conftest import make_image, put16, run_until_halt


def _img(instrs: list, extra: dict = None) -> list:
    img: list = make_image()
    put16(img, 0xFFFE, 0xC000)  # reset vector
    put16(img, 0xFFFC, 0xC020)  # fault vector
    addr: int = 0xC000
    for w in instrs:
        put16(img, addr, w)
        addr += 2
    # Fault handler: read CAUSE, HALT its low byte.
    put16(img, 0xC020, 0x421C)  # RSR CAUSE RA RB
    put16(img, 0xC022, 0x0020)  # HALT RB
    if extra:
        for a, w in extra.items():
            put16(img, a, w)
    return img


def test_disabling_syscall_takes_effect_next_instruction() -> None:
    # EXTNS=0x0F; WSR EXTNS 0x0D clears syscall; the next RSR RETSC must fault illegal.
    img: list = _img([
        0x0039,  # SET RC 0x00
        0x0D49,  # SET RD 0x0D  (dynamichw, privilege, interrupt; syscall off)
        0x543D,  # WSR EXTNS RC RD
        0x821C,  # RSR RETSC RA RB  -> now illegal
    ])
    assert run_until_halt(img, extns_reset=0x0F)["halt_code"] == 0x09


def test_bit0_cannot_be_cleared() -> None:
    # Trying to clear bit 0 leaves Dynamic Hardware on, so a later WSR EXTNS still works.
    img: list = _img([
        0x0039,  # SET RC 0x00
        0x0E49,  # SET RD 0x0E  (attempt to clear bit 0)
        0x543D,  # WSR EXTNS RC RD  -> EXTNS stays 0x0F (bit 0 pinned)
        0x0D49,  # SET RD 0x0D
        0x543D,  # WSR EXTNS RC RD  -> disables syscall (only possible if bit 0 still set)
        0x821C,  # RSR RETSC RA RB  -> illegal
    ])
    assert run_until_halt(img, extns_reset=0x0F)["halt_code"] == 0x09


def test_write_ignored_when_dynamic_hardware_off() -> None:
    # EXTNS=0x00; WSR EXTNS cannot enable syscall, so RSR RETSC stays illegal.
    img: list = _img([
        0x0039,  # SET RC 0x00
        0x0249,  # SET RD 0x02  (attempt to enable syscall)
        0x543D,  # WSR EXTNS RC RD  -> ignored
        0x821C,  # RSR RETSC RA RB  -> illegal
    ])
    assert run_until_halt(img, extns_reset=0x00)["halt_code"] == 0x09
