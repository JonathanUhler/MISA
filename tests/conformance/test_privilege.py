"""
Privilege regression tests for finding A1.

The ISA lets User Mode read PRIVS with RSR while forbidding writes to it. The simulator previously
faulted on a User Mode RSR PRIVS. These tests confirm the read is allowed and the write still
faults. Both require the privilege extension so the assembler recognizes the PRIVS register.

The programs end by faulting on a User Mode HALT, which is expected. Each test reads a register that
survives the fault to tell whether the operation under test was allowed.
"""


from harness import run_program, Reg


def test_rsr_privs_allowed_in_user_mode():
    """User Mode RSR PRIVS returns the ring bit rather than faulting."""

    sim = run_program("priv_rsr.S", extensions = ["privilege"])
    assert sim.reg[Reg.RD] == 0x01


def test_wsr_privs_faults_in_user_mode():
    """User Mode WSR PRIVS faults, so the sentinel after it never runs."""

    sim = run_program("priv_wsr.S", extensions = ["privilege"])
    assert sim.reg[Reg.RE] == 0x00
