from bus import MemoryBus
from ram import Ram
from peripheral import Peripheral


class Probe(Peripheral):

    def __init__(self, base):
        self.base = base
        self.size = 4
        self.regs = [0, 0, 0, 0]
        self.writes = []

    def read(self, offset):
        return self.regs[offset]

    def write(self, offset, value):
        self.writes.append((offset, value))
        self.regs[offset] = value


def test_ram_fallthrough():
    bus = MemoryBus()
    bus.set_backing(Ram())
    bus.write(0x0200, 0x42)
    assert bus.read(0x0200) == 0x42


def test_peripheral_overlay_precedence_and_relative_offset():
    bus = MemoryBus()
    bus.set_backing(Ram())
    p = Probe(0x8000)
    bus.map(p)
    bus.write(0x8001, 0x7A)
    assert p.writes == [(1, 0x7A)]        # dispatched with a base-relative offset
    assert bus.read(0x8001) == 0x7A


def test_address_outside_window_hits_ram():
    bus = MemoryBus()
    ram = Ram()
    bus.set_backing(ram)
    bus.map(Probe(0x8000))
    bus.write(0x7FFF, 0x11)
    assert ram.data[0x7FFF] == 0x11
