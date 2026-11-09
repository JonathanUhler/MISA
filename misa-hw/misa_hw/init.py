from dataclasses import dataclass, field


@dataclass
class GpRegInit:
    ra: int        = field(default_factory = int)
    rb: int        = field(default_factory = int)
    rc: int        = field(default_factory = int)
    rd: int        = field(default_factory = int)
    re: int        = field(default_factory = int)
    rf: int        = field(default_factory = int)
    ru: int        = field(default_factory = int)
    rv: int        = field(default_factory = int)
    rw: int        = field(default_factory = int)
    rx: int        = field(default_factory = int)
    ry: int        = field(default_factory = int)
    rz: int        = field(default_factory = int)
    rt: int        = field(default_factory = int)
    rscratch0: int = field(default_factory = int)
    rscratch1: int = field(default_factory = int)


@dataclass
class CsrRegInit:
    saddr: int = field(default_factory = int)
    raddr: int = field(default_factory = int)
    flags: int = field(default_factory = int)
    cause: int = field(default_factory = int)
    extns: int = field(default_factory = lambda: 0x0F)
    retsc: int = field(default_factory = int)
    retir: int = field(default_factory = int)
    privs: int = field(default_factory = int)


@dataclass
class CoreInit:
    register: GpRegInit  = field(default_factory = GpRegInit)
    special: CsrRegInit  = field(default_factory = CsrRegInit)
    pc: int              = field(default_factory = int)
    in_syscall: bool     = field(default_factory = bool)
    in_interrupt: bool   = field(default_factory = bool)
    int_rscratch: int    = field(default_factory = int)
    int_flags: int       = field(default_factory = int)
    syscall_privs: int   = field(default_factory = int)
    int_privs: int       = field(default_factory = int)
    halted: bool         = field(default_factory = bool)
