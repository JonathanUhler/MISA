from peripheral import Peripheral, pack_le, unpack_le


class Dummy(Peripheral):

    def __init__(self):
        self.base = 0x8000
        self.size = 2
        self.store = [0, 0]

    def read(self, offset):
        return self.store[offset]

    def write(self, offset, value):
        self.store[offset] = value


def test_attach_sets_clock_and_irq():
    d = Dummy()
    d.attach("CLOCK", "IRQ")
    assert d.clock == "CLOCK"
    assert d.irq == "IRQ"


def test_poll_and_reset_default_to_noop():
    d = Dummy()
    assert d.poll() is None
    assert d.reset() is None


def test_pack_and_unpack_le_roundtrip():
    assert pack_le(0xBEEF, 2) == [0xEF, 0xBE]
    assert unpack_le([0xEF, 0xBE]) == 0xBEEF
