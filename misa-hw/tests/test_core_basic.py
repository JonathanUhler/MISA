from conftest import make_image, put16, run_until_halt


def test_set_then_halt_emits_register() -> None:
    # Reset vector -> 0xC000; SET RA 0x2A; HALT RA.
    img: list = make_image()
    put16(img, 0xC000, 0x2A19)  # SET RA(1), imm 0x2A
    put16(img, 0xC002, 0x0010)  # HALT RA(1)
    put16(img, 0xFFFE, 0xC000)  # reset vector
    r: dict = run_until_halt(img)
    assert r["halted"] == 1
    assert r["halt_code"] == 0x2A


def test_set_other_register_and_immediate() -> None:
    # SET RB 0xFF; HALT RB.  RB = 0x2.
    img: list = make_image()
    put16(img, 0xC000, 0xFF29)  # SET RB(2), imm 0xFF  (imm<<8 | rd<<4 | 0x9)
    put16(img, 0xC002, 0x0020)  # HALT RB(2)
    put16(img, 0xFFFE, 0xC000)
    r: dict = run_until_halt(img)
    assert r["halted"] == 1
    assert r["halt_code"] == 0xFF
