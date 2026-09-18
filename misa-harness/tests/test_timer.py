from clock import VirtualClock
from timer import Timer, TimerStruct, TimerCtrlStruct
from constants import TIMER_BASE


class FakeSink:

    def __init__(self):
        self.raised = []

    def raise_irq(self, number, argptr=0):
        self.raised.append((number, argptr))


def _timer(clock, sink):
    t = Timer(TIMER_BASE, irq_number=0x05, cycle_ns=1000)
    t.attach(clock, sink)
    return t


def test_timer_fires_after_reload():
    clock = VirtualClock()
    sink = FakeSink()
    t = _timer(clock, sink)
    t.write(TimerStruct.RELOAD_LO, 3)   # 3 ticks = 3000 ns
    # enabling arms the deadline at now() = 0
    t.write(TimerStruct.CTRL, TimerCtrlStruct.ENABLED | TimerCtrlStruct.IRQ_ENABLED)
    t.poll()
    assert t.read(TimerStruct.STATUS) == 0
    clock.advance(3)                    # now = 3000
    t.poll()
    assert t.read(TimerStruct.STATUS) == 1
    assert sink.raised == [(0x05, 0)]


def test_timer_status_write_clears_expired():
    clock = VirtualClock()
    t = _timer(clock, FakeSink())
    t.write(TimerStruct.RELOAD_LO, 1)
    t.write(TimerStruct.CTRL, TimerCtrlStruct.ENABLED)
    clock.advance(1)
    t.poll()
    assert t.read(TimerStruct.STATUS) == 1
    t.write(TimerStruct.STATUS, 1)
    assert t.read(TimerStruct.STATUS) == 0


def test_timer_auto_reload_fires_repeatedly():
    clock = VirtualClock()
    sink = FakeSink()
    t = _timer(clock, sink)
    t.write(TimerStruct.RELOAD_LO, 2)
    t.write(
        TimerStruct.CTRL,
        TimerCtrlStruct.ENABLED | TimerCtrlStruct.AUTO_ARM | TimerCtrlStruct.IRQ_ENABLED,
    )
    clock.advance(2)
    t.poll()
    clock.advance(2)
    t.poll()
    assert len(sink.raised) == 2
