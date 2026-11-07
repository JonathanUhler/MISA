from misa_hw.init import RegInit, CsrInit

import harness


def _full_state() -> "harness.CosimState":
    return harness.CosimState(
        registers = RegInit(ra = 0x11, rb = 0x22, rc = 0x33, rd = 0x44, re = 0x55, rf = 0x66,
                            ru = 0x77, rv = 0x88, rw = 0x99, rx = 0xAA, ry = 0xBB, rz = 0xCC,
                            rt = 0xDD, rscratch0 = 0xEE, rscratch1 = 0xFF),
        special = CsrInit(saddr = 0x1234, raddr = 0x5678, flags = 0xA, cause = 0xCAFE,
                          extns = 0x0F, retsc = 0x1111, retir = 0x2222, privs = 0x1),
        pc = 0xC0DE, in_syscall = True, in_interrupt = False, syscall_privs = 1,
        int_rscratch = 0x3333, int_flags = 0x5, int_privs = 1, halted = False,
        memory = bytes(range(256)) * 256,
    )


def test_state_round_trip() -> None:
    s: harness.CosimState = _full_state()
    assert harness.parse_state(harness.serialize_state(s)) == s


def test_serialize_gates_extensions() -> None:
    s: harness.CosimState = harness.CosimState(
        registers = RegInit(), special = CsrInit(extns = 0x00), memory = bytes(65536))
    text: str = harness.serialize_state(s)
    for absent in ("inSyscall", "syscallPrivs", "inInterrupt", "intRscratch", "intFlags",
                   "intPrivs", "retsc", "retir", "privs"):
        assert absent not in text, f"{absent} must be gated out when its extension is off"
    for present in ("halted", "pc", "saddr", "raddr", "flags", "cause", "extns"):
        assert present in text, f"{present} must always be serialized"
