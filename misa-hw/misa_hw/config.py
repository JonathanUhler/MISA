"""Typed construction objects shared across the core.

`SpecInit`/`CoreInit` are dataclasses describing an arbitrary initial machine state (used to start the
core from a given state for co-simulation). `StateEnum` is the base for the control FSMs' state
enums: each member's string value is its own name, so Amaranth's `m.FSM(init=...)`, `m.State(...)`,
and `m.next = ...` accept members directly while the emitted state names stay readable.
"""

import enum
from dataclasses import dataclass, field


class StateEnum(enum.StrEnum):
    @staticmethod
    def _generate_next_value_(name: str, start: int, count: int, last_values: list) -> str:
        return name


@dataclass
class SpecInit:
    """Initial values for the special-register file."""
    saddr: int = 0
    raddr: int = 0
    flags: int = 0
    cause: int = 0
    extns: int = 0
    retsc: int = 0
    retir: int = 0
    privs: int = 0


@dataclass
class CoreInit:
    """Initial values for the whole core when starting from an arbitrary state (see Core)."""
    registers: list = field(default_factory=lambda: [0] * 16)
    special: SpecInit = field(default_factory=SpecInit)
    pc: int = 0
    in_syscall: bool = False
    in_interrupt: bool = False
    syscall_privs: int = 0
    int_rscratch: int = 0
    int_flags: int = 0
    int_privs: int = 0
    halted: bool = False
