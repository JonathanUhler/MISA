from conftest import program, run_until_halt


def test_jal_taken_sets_pc_and_raddr() -> None:
    # JAL ALWAYS to 0xC008; RADDR must be 0xC006 (instruction after JAL).
    r: dict = run_until_halt(program([
        0xC019,  # C000 SET RA 0xC0
        0x0829,  # C002 SET RB 0x08  (target 0xC008)
        0x021E,  # C004 JAL ALWAYS RA RB
        0x0000,  # C006 HALT R0  (skipped if taken)
        0x243C,  # C008 RSR RADDR RC RD
        0x0040,  # C00A HALT RD   -> low byte of RADDR = 0x06
    ]))
    assert r["halt_code"] == 0x06


def test_jal_not_taken_falls_through_and_leaves_raddr() -> None:
    # Z=0 at reset, so JAL EQUAL is not taken; RADDR stays 0x0000.
    r: dict = run_until_halt(program([
        0xC019,  # C000 SET RA 0xC0
        0x0C29,  # C002 SET RB 0x0C  (would-be target 0xC00C)
        0x121E,  # C004 JAL EQUAL RA RB  (Z=0 -> not taken)
        0x243C,  # C006 RSR RADDR RC RD
        0x0040,  # C008 HALT RD  -> 0x00 (RADDR unchanged)
        0x0000,  # C00A pad
        0xEE49,  # C00C SET RD 0xEE  (trap if wrongly taken)
        0x0040,  # C00E HALT RD
    ]))
    assert r["halt_code"] == 0x00


def test_jmp_reg_taken() -> None:
    r: dict = run_until_halt(program([
        0xC019,  # C000 SET RA 0xC0
        0x0829,  # C002 SET RB 0x08  (target 0xC008)
        0x021F,  # C004 JMP ALWAYS RA RB
        0x0000,  # C006 HALT R0 (trap)
        0x7739,  # C008 SET RC 0x77
        0x0030,  # C00A HALT RC
    ]))
    assert r["halt_code"] == 0x77


def test_jmp_reg_not_taken() -> None:
    r: dict = run_until_halt(program([
        0xC019,  # C000 SET RA 0xC0
        0x0A29,  # C002 SET RB 0x0A  (would-be target 0xC00A)
        0x1139,  # C004 SET RC 0x11
        0x121F,  # C006 JMP EQUAL RA RB  (Z=0 -> not taken)
        0x0030,  # C008 HALT RC  -> 0x11
        0xEE39,  # C00A SET RC 0xEE (trap)
        0x0030,  # C00C HALT RC
    ]))
    assert r["halt_code"] == 0x11


def test_jmp_csr_returns_to_linked_address() -> None:
    # JAL to subroutine; the subroutine sets RC then JMP ALWAYS RADDR returns to the HALT.
    # Only a real call+return reaches HALT with RC=0x42; a no-op JAL leaves RC=0x00.
    r: dict = run_until_halt(program([
        0xC019,  # C000 SET RA 0xC0
        0x0A29,  # C002 SET RB 0x0A  (subroutine at 0xC00A)
        0x021E,  # C004 JAL ALWAYS RA RB  (RADDR = 0xC006)
        0x0030,  # C006 HALT RC  (return target; RC set only by the subroutine)
        0x0000,  # C008 pad
        0x4239,  # C00A SET RC 0x42
        0x802F,  # C00C JMP ALWAYS RADDR  -> back to 0xC006
    ]))
    assert r["halt_code"] == 0x42


def test_wsr_flags_affects_branch() -> None:
    # WSR FLAGS sets Z=1, so JMP EQUAL is taken (deferred from Task 11).
    r: dict = run_until_halt(program([
        0x0039,  # C000 SET RC 0x00
        0x0149,  # C002 SET RD 0x01
        0x343D,  # C004 WSR FLAGS RC RD  -> FLAGS=0x0001 (Z=1)
        0xC019,  # C006 SET RA 0xC0
        0x1029,  # C008 SET RB 0x10  (target 0xC010)
        0x121F,  # C00A JMP EQUAL RA RB  (Z=1 -> taken)
        0xAA59,  # C00C SET RE 0xAA  (not-taken path)
        0x0050,  # C00E HALT RE
        0x5A59,  # C010 SET RE 0x5A  (taken path)
        0x0050,  # C012 HALT RE
    ]))
    assert r["halt_code"] == 0x5A
