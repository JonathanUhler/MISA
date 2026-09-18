"""
RETS returns from a system-call context to the address in RETSC. Requires the syscall extension so
the assembler recognizes `syscall`/`rets`. The handler here returns immediately.
"""

from harness import run_program, Reg


def test_rets_returns_from_syscall():
    sim = run_program("syscall_rets.S", extensions = ["syscall"])
    assert sim.reg[Reg.RB] == 0x5A
