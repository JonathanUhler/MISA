"""Privilege extension: User Mode restrictions and automatic ring-bit transitions.

Dynamic Hardware + System Call + Privilege are enabled. The processor reaches User Mode by writing
PRIVS bit 0. The fault handler reports the low byte of CAUSE (0x11 = privileged instruction).
"""

import pytest

from conftest import run_program, toolchain_dir

pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")

PRV = ("dynamichw", "syscall", "privilege")

# Enter User Mode: PRIVS.R = 1.
ENTER_USER = """
        set ra 0x00
        set rb 0x01
        wsr privs ra rb
"""

# Machine-mode fault handler that reports CAUSE's low byte.
FAULT_HANDLER = """
    _fault:
        rsr cause ra rb
        halt rb
"""


def test_user_mode_halt_faults() -> None:
    r: dict = run_program(ENTER_USER + "        halt rc\n" + FAULT_HANDLER,
                          extns=PRV, fault="_fault")
    assert r["halt_code"] == 0x11


def test_user_mode_rsr_privileged_csr_faults() -> None:
    # CAUSE is not user-readable.
    r: dict = run_program(ENTER_USER + "        rsr cause rc rd\n" + FAULT_HANDLER,
                          extns=PRV, fault="_fault")
    assert r["halt_code"] == 0x11


def test_user_mode_wsr_privileged_csr_faults() -> None:
    # CAUSE is not user-writable.
    r: dict = run_program(ENTER_USER + "        wsr cause rc rd\n" + FAULT_HANDLER,
                          extns=PRV, fault="_fault")
    assert r["halt_code"] == 0x11


def test_user_mode_allowed_wsr_succeeds() -> None:
    # WSR SADDR is allowed in User Mode; a syscall into Machine Mode reads it back.
    r: dict = run_program(ENTER_USER + """
        set rc 0x00
        set rd 0xab
        wsr saddr rc rd
        syscall re
    _handler:
        rsr saddr ra rb
        halt rb
    """, extns=PRV, syscall="_handler")
    assert r["halt_code"] == 0xAB


def test_syscall_enters_machine_mode() -> None:
    # In the syscall handler the ring bit reads 0 (Machine Mode).
    r: dict = run_program(ENTER_USER + """
        syscall rc
    _handler:
        rsr privs ra rb
        halt rb
    """, extns=PRV, syscall="_handler")
    assert r["halt_code"] == 0x00


def test_syscall_return_restores_user_ring() -> None:
    # After RETS the ring bit is User again, so HALT at the return point faults privileged.
    r: dict = run_program(ENTER_USER + """
        syscall rc
        halt rc
    _handler:
        rets
    _fault:
        rsr cause ra rb
        halt rb
    """, extns=PRV, syscall="_handler", fault="_fault")
    assert r["halt_code"] == 0x11
