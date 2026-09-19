"""
Regression tests for misa-ld section placement.

A memory region that lists more than one section name (such as `text rodata` or `data bss` in the
default map) must concatenate those sections in name-list order, with the running offset carried
across names, rather than restarting each name at the region base. Before the fix both sections
began at the same address and overlapped, so a rodata constant aliased code and a bss write clobbered
initialized data. These programs would fail under that behavior.
"""


from harness import run_source, Csr, Reg


VECTORS_FOOTER = "        .section vectors\n        .space 14\n        .addr _start\n"


def test_text_and_rodata_do_not_overlap():
    """A rodata constant sharing the text region is placed after text and reads back intact."""

    source = (
        "        .section text\n"
        "_start:\n"
        "        set2 rscratch rodata_val\n"
        "        ld rt rscratch\n"
        "        halt rt\n"
        "        .section rodata\n"
        "rodata_val:\n"
        "        .word 0xAB\n"
        + VECTORS_FOOTER
    )
    sim = run_source(source)
    assert sim.reg[Reg.RT] == 0xAB
    assert sim.get_csr(Csr.CAUSE) == (0xAB << 8) | 0x01


def test_data_and_bss_do_not_overlap():
    """A bss cell sharing the data region sits after data, so writing it leaves data untouched."""

    source = (
        "        .section text\n"
        "_start:\n"
        "        set2 rc rd scratch\n"
        "        set ra 0x99\n"
        "        st ra rc rd\n"           # clobber the bss cell
        "        set2 rscratch data_val\n"
        "        ld rt rscratch\n"        # data must be intact
        "        halt rt\n"
        "        .section data\n"
        "data_val:\n"
        "        .word 0xCD\n"
        "        .section bss\n"
        "scratch:\n"
        "        .space 1\n"
        + VECTORS_FOOTER
    )
    sim = run_source(source)
    assert sim.reg[Reg.RT] == 0xCD
    assert sim.get_csr(Csr.CAUSE) == (0xCD << 8) | 0x01
