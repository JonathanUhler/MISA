"""
Tests for the JMP-to-CSR encoding form (design §1.3). CSR form: byte0 = 0xF | CSR<<4,
byte1 = CMP<<4 | 0x80 (bit 15 set). RADDR's CSR index is 0x2.
"""

from harness import Simulator, Reg, Csr, run


def test_jmp_csr_form_branches_to_special_register():
    """A hand-assembled JMP ALWAYS RADDR jumps to the value held in RADDR."""
    sim = Simulator()
    sim.set_csr(Csr.RADDR, 0x1234)
    sim.mem[0x0000] = 0xF | (int(Csr.RADDR) << 4)   # 0x2F
    sim.mem[0x0001] = (int(0) << 4) | 0x80          # CMP=ALWAYS, selector=1 -> 0x80
    sim.pc = 0x0000
    sim.in_reset = False
    sim.step()
    assert sim.pc == 0x1234


def test_jmp_csr_form_assembles_and_runs():
    """End to end: `jmp always raddr` returns to a seeded RADDR (equivalent to RET)."""
    sim = run("""
        set2 rc rd target
        wsr raddr rc rd
        jmp always raddr
        set ra 0xEE
        halt ra
    target:
        set ra 0x2A
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
