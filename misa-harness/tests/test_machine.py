from clock import VirtualClock
from machine import Machine
from timer import Timer, TimerStruct, TimerCtrlStruct
from constants import IRQ_BASE, TIMER_BASE


def test_ram_is_the_default_backing():
    m = Machine(VirtualClock())
    m.bus.write(0x0200, 0x55)
    assert m.bus.read(0x0200) == 0x55


def test_interrupt_controller_reachable_over_the_bus():
    m = Machine(VirtualClock())
    m.irq.raise_irq(0x07, 0xBEEF)
    assert m.bus.read(IRQ_BASE + 0) == 1          # length
    assert m.bus.read(IRQ_BASE + 3) == 0x07       # head number


def test_poll_fans_out_and_timer_raises_through_shared_sink():
    clock = VirtualClock()
    m = Machine(clock)
    t = Timer(TIMER_BASE, irq_number=0x05, cycle_ns=1000)
    m.add(t)
    m.bus.write(TIMER_BASE + TimerStruct.RELOAD_LO, 2)
    m.bus.write(
        TIMER_BASE + TimerStruct.CTRL,
        TimerCtrlStruct.ENABLED | TimerCtrlStruct.IRQ_ENABLED,
    )
    clock.advance(2)
    m.poll()
    assert m.irq.pending() is True
    assert m.bus.read(IRQ_BASE + 3) == 0x05       # the timer's number is queued
