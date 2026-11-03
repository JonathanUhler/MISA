"""Combinational instruction decoder.

Slices the 16-bit instruction word into the opcode, operand nibbles, immediates, comparison, and the
bit-15 target/syscall selector, and flags structurally illegal encodings. Extension gating of CSRs
is applied later by the control FSM where the live EXTNS is known.
"""

from amaranth import Module, Value
from amaranth.lib import wiring, data
from amaranth.lib.wiring import In, Out

from .types import Opcode


DecodedLayout: data.StructLayout = data.StructLayout({
    "opcode": Opcode,
    "n1": 4,
    "n2": 4,
    "n3": 4,
    "imm8": 8,
    "imm4": 4,
    "cmp": 3,
    "csr_target": 1,
    "syscall_bit": 1,
    "illegal": 1,
})


def _valid_csr(x: Value) -> Value:
    return ((x == 1) | (x == 2) | (x == 3) | (x == 4) | (x == 5)
            | (x == 8) | (x == 9) | (x == 10))


class Decoder(wiring.Component):
    inst: In(16)
    out: Out(DecodedLayout)

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()
        inst: Value = self.inst
        opcode: Value = inst[0:4]
        n1: Value = inst[4:8]
        n2: Value = inst[8:12]
        n3: Value = inst[12:16]
        cmp: Value = inst[12:15]
        csr_target: Value = inst[15]

        m.d.comb += [
            self.out.opcode.eq(opcode),
            self.out.n1.eq(n1),
            self.out.n2.eq(n2),
            self.out.n3.eq(n3),
            self.out.imm8.eq(inst[8:16]),
            self.out.imm4.eq(inst[12:16]),
            self.out.cmp.eq(cmp),
            self.out.csr_target.eq(csr_target),
            self.out.syscall_bit.eq(inst[15]),
        ]

        rsr_wsr: Value = (opcode == Opcode.RSR) | (opcode == Opcode.WSR)
        jmp_to_csr: Value = (opcode == Opcode.JMP) & csr_target
        has_cmp: Value = (opcode == Opcode.JAL) | (opcode == Opcode.JMP)
        m.d.comb += self.out.illegal.eq(
            (rsr_wsr & ~_valid_csr(n3))
            | (jmp_to_csr & ~_valid_csr(n1))
            | (has_cmp & (cmp == 0b111))
        )
        return m
