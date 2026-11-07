import pytest

from misa_hw.init import RegInit, CsrInit

import harness
from testlib import assemble, extns_bits, spec_exe, toolchain_dir

pytestmark = pytest.mark.skipif(
    spec_exe() is None or toolchain_dir() is None,
    reason = "misa-spec-exe oracle and/or misa-as toolchain not found"
)


def _cosim(text: str, exts: tuple = (), n: int = 50, **kw) -> None:
    img: list = assemble(text, extns = exts, **kw)
    state: harness.CosimState = harness.reset_state(img, extns_bits(exts))
    harness.lockstep(state, n)


def _raw(words: dict, pc: int, extns: int = 0, regs: RegInit = None, n: int = 8) -> None:
    mem: bytearray = bytearray(65536)
    for addr, word in words.items():
        mem[addr] = word & 0xFF
        mem[addr + 1] = (word >> 8) & 0xFF
    state: harness.CosimState = harness.CosimState(
        registers = regs if regs is not None else RegInit(),
        special = CsrInit(extns = extns), pc = pc, memory = bytes(mem)
    )
    harness.lockstep(state, n)


def test_alu_and_flags() -> None:
    _cosim("""
        set ra 0x05
        set rb 0x03
        add rc ra rb
        sub rd ra rb
        and re ra rb
        or  rf ra rb
        xor ru ra rb
        rrc rv ra
        halt rc
    """)


def test_set_and_halt() -> None:
    _cosim("""
        set ra 0x2a
        set rb 0xff
        halt rb
    """)


def test_load_store_reg_form() -> None:
    _cosim("""
        set ra 0x02
        set rb 0x10
        set rc 0x5a
        st rc ra 0
        set rc 0x00
        ld rd ra 0
        halt rd
    """)


def test_stack_push_pop() -> None:
    _cosim("""
        set2 rscratch 0x01ff
        wsr saddr rscratch
        set ra 0xaa
        push ra
        pop rb
        halt rb
    """)


def test_csr_wsr_rsr() -> None:
    _cosim("""
        set ra 0x12
        set rb 0x34
        wsr saddr ra rb
        rsr saddr rc rd
        halt rc
    """)


def test_branch_jal_jmp_taken_and_not_taken() -> None:
    _cosim("""
        jali always _a
        halt r0
    _a:
        jali equal _b
        set rc 0x11
        jmpi always _c
    _b:
        set rc 0xee
    _c:
        halt rc
    """)


def test_call_and_ret() -> None:
    _cosim("""
        call _sub
        halt rc
    _sub:
        set rc 0x42
        ret
    """)


def test_syscall_entry_and_return() -> None:
    _cosim("""
        set ra 0x07
        syscall ra
        set rc 0x55
        halt rc
    _handler:
        rets
    """, exts = ("dynamichw", "syscall"), syscall = "_handler")


def test_privilege_user_mode_halt_faults() -> None:
    _cosim("""
        set ra 0x00
        set rb 0x01
        wsr privs ra rb
        halt rc
    _fault:
        rsr cause ra rb
        halt rb
    """, exts = ("dynamichw", "syscall", "privilege"), fault = "_fault")


def test_dynamichw_extns_roundtrip() -> None:
    _cosim("""
        set ra 0x00
        set rb 0x0f
        wsr extns ra rb
        rsr extns rc rd
        halt rd
    """, exts = ("dynamichw",))


def test_illegal_instruction_faults() -> None:
    _raw({0xC000: 0x600C, 0xFFFC: 0x0000, 0x0000: 0x0000}, pc = 0xC000)


def test_pc_overflow_faults() -> None:
    _raw({0xFFFE: 0x0009, 0xFFFC: 0x0000, 0x0000: 0x0000}, pc = 0xFFFE)
