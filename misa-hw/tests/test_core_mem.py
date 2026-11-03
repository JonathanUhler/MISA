from conftest import program, run_until_halt


def test_round_trip_store_then_load_reg_form() -> None:
    # ptr {RA,RB} = 0x0210; ST RC there; clobber RC; LD RD back; HALT RD -> 0x5A
    r: dict = run_until_halt(program([
        0x0219,  # SET RA 0x02
        0x1029,  # SET RB 0x10
        0x5A39,  # SET RC 0x5A
        0x013B,  # ST RC [RA:RB]+0
        0x0039,  # SET RC 0x00
        0x014A,  # LD RD [RA:RB]+0
        0x0040,  # HALT RD
    ]))
    assert r["halt_code"] == 0x5A


def test_store_with_negative_offset_sign_extends() -> None:
    # base {0x02,0x11}=0x0211, ST RC at -1 -> 0x0210; load 0x0210 via base {0x02,0x10}
    r: dict = run_until_halt(program([
        0x0219,  # SET RA 0x02
        0x1129,  # SET RB 0x11
        0x7E39,  # SET RC 0x7E
        0xF13B,  # ST RC [RA:RB]-1  -> addr 0x0210
        0x1029,  # SET RB 0x10
        0x014A,  # LD RD [RA:RB]+0  -> addr 0x0210
        0x0040,  # HALT RD
    ]))
    assert r["halt_code"] == 0x7E


def test_base_pair_low_register_wraps() -> None:
    # rsb = RSCRATCH0 (0xE); low byte from reg (0xE+1)=0xF = RSCRATCH1
    r: dict = run_until_halt(program([
        0x03E9,  # SET RSCRATCH0 0x03
        0x20F9,  # SET RSCRATCH1 0x20   -> addr 0x0320
        0x3319,  # SET RA 0x33
        0x0E1B,  # ST RA [RSCRATCH0:RSCRATCH1]+0
        0x0019,  # SET RA 0x00
        0x0E2A,  # LD RB [RSCRATCH0:RSCRATCH1]+0
        0x0020,  # HALT RB
    ]))
    assert r["halt_code"] == 0x33


def test_stack_push_then_pop() -> None:
    # PUSH RA, PUSH RB (ST stack, imm -1); POP RC (LD stack, imm +1) -> last pushed = 0xBB
    r: dict = run_until_halt(program([
        0xAA19,  # SET RA 0xAA
        0xBB29,  # SET RB 0xBB
        0xFF1B,  # ST RA [stack] -1  (PUSH RA)
        0xFF2B,  # ST RB [stack] -1  (PUSH RB)
        0x1F3A,  # LD RC [stack] +1  (POP RC)
        0x0030,  # HALT RC
    ]))
    assert r["halt_code"] == 0xBB
