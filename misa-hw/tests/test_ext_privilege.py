"""Privilege extension: User Mode restrictions and automatic ring-bit transitions.

EXTNS=0x07 (dynamichw + syscall + privilege). The processor reaches User Mode by WSR PRIVS with
bit 0 set. The fault handler reports the low byte of CAUSE (0x11 = privileged instruction).
"""

from conftest import make_image, put16, run_until_halt

PRV: int = 0x07


def _base(main: list) -> list:
    img: list = make_image()
    put16(img, 0xFFFE, 0xC000)
    addr: int = 0xC000
    for w in main:
        put16(img, addr, w)
        addr += 2
    return img


def _fault_handler(img: list, at: int = 0xC050) -> None:
    put16(img, 0xFFFC, at)
    put16(img, at, 0x421C)      # RSR CAUSE RA RB
    put16(img, at + 2, 0x0020)  # HALT RB


# Enter User Mode: SET RA 0x00 ; SET RB 0x01 ; WSR PRIVS RA RB
ENTER_USER: list = [0x0019, 0x0129, 0xA21D]


def test_user_mode_halt_faults() -> None:
    img: list = _base(ENTER_USER + [0x0030])  # HALT RC in User Mode
    _fault_handler(img)
    assert run_until_halt(img, extns_reset=PRV)["halt_code"] == 0x11


def test_user_mode_rsr_privileged_csr_faults() -> None:
    img: list = _base(ENTER_USER + [0x443C])  # RSR CAUSE RC RD (CAUSE not user-readable)
    _fault_handler(img)
    assert run_until_halt(img, extns_reset=PRV)["halt_code"] == 0x11


def test_user_mode_wsr_privileged_csr_faults() -> None:
    img: list = _base(ENTER_USER + [0x443D])  # WSR CAUSE RC RD (CAUSE not user-writable)
    _fault_handler(img)
    assert run_until_halt(img, extns_reset=PRV)["halt_code"] == 0x11


def test_user_mode_allowed_wsr_succeeds() -> None:
    # WSR SADDR is allowed in User Mode; SYSCALL to a machine-mode handler reads it back.
    img: list = _base(ENTER_USER + [
        0x0039,  # SET RC 0x00
        0xAB49,  # SET RD 0xAB
        0x143D,  # WSR SADDR RC RD  (allowed) -> SADDR = 0x00AB
        0x8050,  # SYSCALL RE
    ])
    put16(img, 0xFFFA, 0xC030)
    put16(img, 0xC030, 0x121C)  # RSR SADDR RA RB
    put16(img, 0xC032, 0x0020)  # HALT RB -> 0xAB
    assert run_until_halt(img, extns_reset=PRV)["halt_code"] == 0xAB


def test_syscall_enters_machine_mode() -> None:
    # In the syscall handler the ring bit must read 0 (Machine Mode).
    img: list = _base(ENTER_USER + [0x8030])  # SYSCALL RC
    put16(img, 0xFFFA, 0xC030)
    put16(img, 0xC030, 0xA21C)  # RSR PRIVS RA RB
    put16(img, 0xC032, 0x0020)  # HALT RB -> 0x00
    assert run_until_halt(img, extns_reset=PRV)["halt_code"] == 0x00


def test_syscall_return_restores_user_ring() -> None:
    # After RETS, the ring bit is User again, so HALT at the return point faults privileged.
    img: list = _base(ENTER_USER + [
        0x8030,  # C006 SYSCALL RC  (RETSC = 0xC008)
        0x0030,  # C008 HALT RC  (return point, User Mode -> privileged fault)
    ])
    put16(img, 0xFFFA, 0xC030)
    put16(img, 0xC030, 0x808F)  # JMP ALWAYS RETSC
    _fault_handler(img)
    assert run_until_halt(img, extns_reset=PRV)["halt_code"] == 0x11
