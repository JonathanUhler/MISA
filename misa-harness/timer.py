from enum import IntEnum

from constants import WORD_MASK, TIMER_BASE
from peripheral import Peripheral


class TimerStruct(IntEnum):
    CTRL: int      = 0
    STATUS: int    = 1
    RELOAD_LO: int = 2
    RELOAD_HI: int = 3


class TimerCtrlStruct(IntEnum):
    ENABLED: int     = 0b001
    AUTO_ARM: int    = 0b010
    IRQ_ENABLED: int = 0b100


class Timer(Peripheral):

    def __init__(
            self, base: int = TIMER_BASE, irq_number: int = 0x01, cycle_ns: int = 1000
    ) -> None:
        self.base = base
        self.size = 4
        self._irq_number = irq_number
        self._cycle_ns = cycle_ns
        self.reset()


    def reset(self) -> None:
        self._ctrl = 0
        self._expired = 0
        self._reload = 0
        self._deadline = 0


    def _arm(self) -> None:
        self._deadline = self.clock.now() + self._reload * self._cycle_ns


    def read(self, offset: int) -> int:
        if (offset == TimerStruct.CTRL):
            return self._ctrl
        if (offset == TimerStruct.STATUS):
            return self._expired
        if (offset == TimerStruct.RELOAD_LO):
            return self._reload & WORD_MASK
        if (offset == TimerStruct.RELOAD_HI):
            return (self._reload >> 8) & WORD_MASK
        return 0


    def write(self, offset: int, value: int) -> None:
        value &= WORD_MASK
        if (offset == TimerStruct.CTRL):
            was_enabled: bool = self._ctrl & TimerCtrlStruct.ENABLED
            self._ctrl = value
            if ((value & TimerCtrlStruct.ENABLED) and not was_enabled):
                self._arm()
        elif (offset == TimerStruct.STATUS):
            if (value & 0b1):
                self._expired = 0
        elif (offset == TimerStruct.RELOAD_LO):
            self._reload = (self._reload & 0xFF00) | value
        elif (offset == TimerStruct.RELOAD_HI):
            self._reload = (self._reload & 0x00FF) | (value << 8)


    def poll(self) -> None:
        if (not (self._ctrl & TimerCtrlStruct.ENABLED)):
            return
        if (self.clock.now() < self._deadline):
            return
        self._expired = 1
        if (self._ctrl & TimerCtrlStruct.IRQ_ENABLED):
            self.irq.raise_irq(self._irq_number)
        if (self._ctrl & TimerCtrlStruct.AUTO_ARM):
            self._arm()
        else:
            self._ctrl &= ~TimerCtrlStruct.ENABLED
