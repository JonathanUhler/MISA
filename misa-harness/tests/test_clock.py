from clock import VirtualClock


def test_virtual_clock_starts_at_zero():
    assert VirtualClock().now() == 0


def test_virtual_clock_advances_by_period():
    c = VirtualClock(hz=1_000_000)   # 1000 ns per cycle
    c.advance()
    assert c.now() == 1000
    c.advance(3)
    assert c.now() == 4000


def test_virtual_clock_reset():
    c = VirtualClock()
    c.advance(5)
    c.reset()
    assert c.now() == 0
