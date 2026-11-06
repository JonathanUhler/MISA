from dataclasses import dataclass, field


@dataclass
class CsrInit:
    saddr: int = field(default_factory = int)
    raddr: int = field(default_factory = int)
    flags: int = field(default_factory = int)
    cause: int = field(default_factory = int)
    extns: int = field(default_factory = lambda: 0x0F)
    retsc: int = field(default_factory = int)
    retir: int = field(default_factory = int)
    privs: int = field(default_factory = int)


@dataclass
class RegInit:
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
