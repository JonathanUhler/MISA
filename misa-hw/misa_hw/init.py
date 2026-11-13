from dataclasses import dataclass, field


@dataclass
class GpRegInit:
    ra: int        = field(default = 0x00)
    rb: int        = field(default = 0x00)
    rc: int        = field(default = 0x00)
    rd: int        = field(default = 0x00)
    re: int        = field(default = 0x00)
    rf: int        = field(default = 0x00)
    ru: int        = field(default = 0x00)
    rv: int        = field(default = 0x00)
    rw: int        = field(default = 0x00)
    rx: int        = field(default = 0x00)
    ry: int        = field(default = 0x00)
    rz: int        = field(default = 0x00)
    rt: int        = field(default = 0x00)
    rscratch0: int = field(default = 0x00)
    rscratch1: int = field(default = 0x00)


@dataclass
class CsrRegInit:
    saddr: int = field(default = 0x0000)
    raddr: int = field(default = 0x0000)
    flags: int = field(default = 0x0000)
    cause: int = field(default = 0x0000)
    extns: int = field(default = 0x000F)
    fault: int = field(default = 0x0000)
    retsc: int = field(default = 0x0000)
    retir: int = field(default = 0x0000)
    privs: int = field(default = 0x0000)


@dataclass
class CoreInit:
    register: GpRegInit  = field(default_factory = GpRegInit)
    special: CsrRegInit  = field(default_factory = CsrRegInit)
    pc: int              = field(default = 0x0000)
    in_syscall: bool     = field(default = False)
    in_interrupt: bool   = field(default = False)
    int_pending: bool    = field(default = False)
    int_rscratch: int    = field(default = 0x0000)
    int_flags: int       = field(default = 0x0000)
    syscall_privs: int   = field(default = 0x0000)
    int_privs: int       = field(default = 0x0000)
    halted: bool         = field(default = False)
