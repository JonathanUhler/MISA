"""System Call extension: SYSCALL entry, RETSC return, and the control-event faults.

Programs run with EXTNS=0x03 (Dynamic Hardware + System Call) so privilege/interrupt stay off and the
processor remains in Machine Mode. The fault handler reports the low byte of CAUSE.
"""

from conftest import make_image, put16, run_until_halt

SYS: int = 0x03  # dynamichw + syscall


def _base(main: list) -> list:
    img: list = make_image()
    put16(img, 0xFFFE, 0xC000)
    addr: int = 0xC000
    for w in main:
        put16(img, addr, w)
        addr += 2
    return img


def _cause_halt_handler(img: list, at: int) -> None:
    put16(img, at, 0x421C)      # RSR CAUSE RA RB
    put16(img, at + 2, 0x0020)  # HALT RB  (low byte of CAUSE)


def test_syscall_enters_handler_and_sets_cause() -> None:
    # HALT the CAUSE low byte (0x01): a SYSCALL-as-HALT bug would emit RA (0x07), not 0x01, so this
    # proves both the jump to the handler and that CAUSE was set to a syscall cause.
    img: list = _base([0x0719, 0x8010])  # SET RA 0x07 ; SYSCALL RA
    put16(img, 0xFFFA, 0xC030)     # syscall handler
    put16(img, 0xC030, 0x421C)     # RSR CAUSE RA RB
    put16(img, 0xC032, 0x0020)     # HALT RB  (low byte of CAUSE = 0x01)
    assert run_until_halt(img, extns_reset=SYS)["halt_code"] == 0x01


def test_syscall_sets_retsc_to_following_instruction() -> None:
    img: list = _base([0x0719, 0x8010])  # SET RA 0x07 ; SYSCALL RA  (at 0xC002, next = 0xC004)
    put16(img, 0xFFFA, 0xC030)
    put16(img, 0xC030, 0x821C)     # RSR RETSC RA RB
    put16(img, 0xC032, 0x0020)     # HALT RB  (low byte of RETSC = 0x04)
    assert run_until_halt(img, extns_reset=SYS)["halt_code"] == 0x04


def test_syscall_return_leaves_context() -> None:
    img: list = _base([
        0x0719,  # C000 SET RA 0x07
        0x8010,  # C002 SYSCALL RA   (RETSC = 0xC004)
        0x5539,  # C004 SET RC 0x55  (return lands here)
        0x0030,  # C006 HALT RC
    ])
    put16(img, 0xFFFA, 0xC030)
    put16(img, 0xC030, 0x808F)     # JMP ALWAYS RETSC  (return)
    assert run_until_halt(img, extns_reset=SYS)["halt_code"] == 0x55


def test_conditional_context_return_faults() -> None:
    img: list = _base([0x0719, 0x8010])  # SET RA 0x07 ; SYSCALL RA
    put16(img, 0xFFFA, 0xC030)
    put16(img, 0xC030, 0x0003)     # SUB R0 R0 R0  -> Z=1
    put16(img, 0xC032, 0x908F)     # JMP EQUAL RETSC  (check true, cmp != ALWAYS) -> fault
    put16(img, 0xFFFC, 0xC050)     # fault vector
    _cause_halt_handler(img, 0xC050)
    assert run_until_halt(img, extns_reset=SYS)["halt_code"] == 0x0B


def test_improper_return_context_faults() -> None:
    img: list = _base([0x808F])          # JMP ALWAYS RETSC with no active context
    put16(img, 0xFFFC, 0xC050)
    _cause_halt_handler(img, 0xC050)
    assert run_until_halt(img, extns_reset=SYS)["halt_code"] == 0x03


def test_nested_syscall_faults() -> None:
    img: list = _base([0x0719, 0x8010])  # SET RA 0x07 ; SYSCALL RA
    put16(img, 0xFFFA, 0xC030)
    put16(img, 0xC030, 0x8020)     # SYSCALL RB  (inside syscall context) -> fault
    put16(img, 0xFFFC, 0xC050)
    _cause_halt_handler(img, 0xC050)
    assert run_until_halt(img, extns_reset=SYS)["halt_code"] == 0x13


def test_syscall_behaves_as_halt_when_extension_disabled() -> None:
    img: list = _base([0x2A19, 0x8010])  # SET RA 0x2A ; SYSCALL RA
    r: dict = run_until_halt(img, extns_reset=0x00)
    assert r["halted"] == 1 and r["halt_code"] == 0x2A
