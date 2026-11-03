from conftest import program, run_until_halt


def test_add() -> None:
    # SET RA 0x05; SET RB 0x03; ADD RC RA RB; HALT RC -> 0x08
    r: dict = run_until_halt(program([0x0519, 0x0329, 0x2131, 0x0030]))
    assert r["halt_code"] == 0x08


def test_adc_uses_prior_carry_and_set_preserves_flags() -> None:
    # SET RA 0xFF; SET RB 0x01; ADD RC RA RB (-> 0x00, C=1);
    # SET RD 0x07 (non-ALU, must not clear flags); ADC RE R0 R0 (= C); HALT RE -> 0x01
    r: dict = run_until_halt(program([0xFF19, 0x0129, 0x2131, 0x0749, 0x0052, 0x0050]))
    assert r["halt_code"] == 0x01


def test_rrc_chain() -> None:
    # SET RA 0x02; RRC RB RA (C=0 -> 0x01, C=0); RRC RC RB (-> 0x00, C=1);
    # RRC RD RC (C=1 -> 0x80); HALT RD -> 0x80
    r: dict = run_until_halt(program([0x0219, 0x0128, 0x0238, 0x0348, 0x0040]))
    assert r["halt_code"] == 0x80


def test_write_to_r0_discarded() -> None:
    # SET RA 0x05; SET RB 0x03; ADD R0 RA RB (discarded);
    # ADD RC R0 RB (= 0 + 3); HALT RC -> 0x03
    r: dict = run_until_halt(program([0x0519, 0x0329, 0x2101, 0x2031, 0x0030]))
    assert r["halt_code"] == 0x03


def test_and_then_or() -> None:
    # SET RA 0xF0; SET RB 0x0F; AND RC RA RB (-> 0x00); OR RD RA RB (-> 0xFF); HALT RD -> 0xFF
    r: dict = run_until_halt(program([0xF019, 0x0F29, 0x2135, 0x2146, 0x0040]))
    assert r["halt_code"] == 0xFF
