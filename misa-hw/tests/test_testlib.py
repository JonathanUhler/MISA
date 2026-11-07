import pytest

from testlib import assemble, put16, run_on_hw, toolchain_dir


def test_run_on_hw_hand_encoded() -> None:
    img: list = [0] * 65536
    put16(img, 0x0000, 0x2A19)   # SET RA 0x2A
    put16(img, 0x0002, 0x0010)   # HALT RA
    img[0xFFFE] = 0x00
    img[0xFFFF] = 0x00           # reset vector -> 0x0000
    out: dict = run_on_hw(img)
    assert out["halted"] == 1
    assert out["halt_code"] == 0x2A


@pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")
def test_assemble_runs() -> None:
    img: list = assemble("        set ra 0x2a\n        halt ra\n")
    assert run_on_hw(img)["halt_code"] == 0x2A
