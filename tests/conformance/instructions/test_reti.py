"""
Assembly-level tests for the Interrupt Extension surface: `reti` and the `retir` CSR are gated on
`-e interrupt`, and `reti` assembles to `JMP ALWAYS RETIR`. Execution is covered in the interrupts
suite once the simulator takes interrupts.
"""

import tempfile

from harness import assemble, Csr

TEXT_BASE = 0xC000  # the default linker map places the `text` section at 0xC000


def test_reti_requires_interrupt_extension():
    with tempfile.TemporaryDirectory() as workdir:
        # Without the extension, `reti` is not a known mnemonic and assembly must fail.
        try:
            assemble("        .section text\n_start:\n        reti\n", workdir)
        except AssertionError:
            return
        raise AssertionError("expected assembly of `reti` to fail without -e interrupt")


def test_reti_assembles_with_interrupt_extension():
    src = ("        .section text\n_start:\n        reti\n"
           "        .section vectors\n        .space 14\n        .addr _start\n")
    with tempfile.TemporaryDirectory() as workdir:
        bin_path = assemble(src, workdir, extensions = ["interrupt"])
        data = bin_path.read_bytes()
    # RETI -> JMP ALWAYS RETIR: byte0 = 0xF | RETIR<<4 = 0x9F, byte1 = 0x80.
    assert data[TEXT_BASE + 0] == (0xF | (int(Csr.RETIR) << 4))
    assert data[TEXT_BASE + 1] == 0x80
