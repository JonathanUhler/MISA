from conftest import program, run_until_halt


def test_wsr_then_rsr_high_byte_to_rs1() -> None:
    # WSR SADDR RA RB (SADDR=0x1234); RSR SADDR RC RD; RC gets high byte 0x12
    r: dict = run_until_halt(program([
        0x1219,  # SET RA 0x12
        0x3429,  # SET RB 0x34
        0x121D,  # WSR SADDR RA RB
        0x143C,  # RSR SADDR RC RD
        0x0030,  # HALT RC
    ]))
    assert r["halt_code"] == 0x12


def test_wsr_then_rsr_low_byte_to_rs2() -> None:
    # same prefix; RD gets low byte 0x34
    r: dict = run_until_halt(program([
        0x1219,  # SET RA 0x12
        0x3429,  # SET RB 0x34
        0x121D,  # WSR SADDR RA RB
        0x143C,  # RSR SADDR RC RD
        0x0040,  # HALT RD
    ]))
    assert r["halt_code"] == 0x34


def test_wsr_flags_direct_write_then_rsr() -> None:
    # WSR FLAGS writes FLAGS via the direct path; RSR FLAGS reads it back (low nibble 0x0A).
    r: dict = run_until_halt(program([
        0x0019,  # SET RA 0x00
        0x0A29,  # SET RB 0x0A
        0x321D,  # WSR FLAGS RA RB  -> FLAGS = 0x000A
        0x343C,  # RSR FLAGS RC RD
        0x0040,  # HALT RD
    ]))
    assert r["halt_code"] == 0x0A
