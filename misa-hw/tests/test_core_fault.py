from conftest import make_image, put16, run_until_halt


def test_illegal_instruction_sets_cause_and_vectors() -> None:
    # Illegal RSR (CSR field 0x6) at 0xC000 -> fault; handler reads CAUSE low byte = 0x09.
    img: list = make_image()
    put16(img, 0xFFFE, 0xC000)  # reset vector
    put16(img, 0xC000, 0x600C)  # illegal RSR (CSR id 0x6)
    put16(img, 0xFFFC, 0xC010)  # fault vector
    put16(img, 0xC010, 0x421C)  # RSR CAUSE RA RB
    put16(img, 0xC012, 0x0020)  # HALT RB
    r: dict = run_until_halt(img)
    assert r["halt_code"] == 0x09


def test_illegal_instruction_discards_partial_result() -> None:
    # RA = 0x5A, then an illegal RSR whose rs1 is RA; if preempted, RA keeps 0x5A.
    img: list = make_image()
    put16(img, 0xFFFE, 0xC000)
    put16(img, 0xC000, 0x5A19)  # SET RA 0x5A
    put16(img, 0xC002, 0x621C)  # illegal RSR (CSR 0x6), rs1 = RA
    put16(img, 0xFFFC, 0xC010)  # fault vector
    put16(img, 0xC010, 0x0010)  # HALT RA
    r: dict = run_until_halt(img)
    assert r["halt_code"] == 0x5A


def test_pc_overflow_faults() -> None:
    # Reset vector value 0xC009 doubles as a sequential SET at 0xFFFE; jumping there and advancing
    # wraps PC past 0xFFFF and must raise the PC-overflow fault (CAUSE low byte 0x0A).
    img: list = make_image()
    put16(img, 0xFFFE, 0xC009)  # reset vector -> 0xC009; word at 0xFFFE = SET R0 0xC0
    put16(img, 0xC009, 0xFF19)  # SET RA 0xFF
    put16(img, 0xC00B, 0xFE29)  # SET RB 0xFE   (pair = 0xFFFE)
    put16(img, 0xC00D, 0x021F)  # JMP ALWAYS RA RB -> 0xFFFE
    put16(img, 0xFFFC, 0xC020)  # fault vector
    put16(img, 0xC020, 0x421C)  # RSR CAUSE RA RB
    put16(img, 0xC022, 0x0020)  # HALT RB
    r: dict = run_until_halt(img)
    assert r["halt_code"] == 0x0A
