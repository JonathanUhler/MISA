from ram import Ram


def test_ram_size_and_base():
    r = Ram(0x0000, 0x10000)
    assert r.base == 0x0000
    assert r.size == 0x10000
    assert len(r.data) == 0x10000


def test_ram_read_write_roundtrip():
    r = Ram()
    r.write(0x1234, 0x5A)
    assert r.read(0x1234) == 0x5A


def test_ram_write_masks_to_byte():
    r = Ram()
    r.write(0x0000, 0x1FF)
    assert r.read(0x0000) == 0xFF
