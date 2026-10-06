from abc import ABC, abstractmethod
from enum import IntEnum

from peripheral import Peripheral
from constants import IRQ_BASE, WORD_MASK, ADDR_MASK


class InterruptSink(ABC):

    @abstractmethod
    def raise_irq(self, number: int, argptr: int = 0) -> None:
        """Request an interrupt with a number and an argument pointer."""


class InterruptStruct(IntEnum):
    LENGTH: int    = 0
    POP: int       = 1
    ACK: int       = 2
    NUMBER: int    = 3
    ARGPTR_LO: int = 4
    ARGPTR_HI: int = 5


class InterruptController(Peripheral, InterruptSink):

    def __init__(self, base: int = IRQ_BASE) -> None:
        self.base = base
        self.size = 6
        self.reset()


    def reset(self) -> None:
        self._fifo = []
        self._ack = 0
        self._prev_pop = 0
        self._line = False


    def raise_irq(self, number: int, argptr: int = 0) -> None:
        self._fifo.append((number & WORD_MASK, argptr & ADDR_MASK))
        self._line = True


    def pending(self) -> bool:
        return self._line


    def acknowledge(self) -> None:
        self._line = False


    def read(self, offset: int) -> int:
        if (offset == InterruptStruct.LENGTH):
            return len(self._fifo) & WORD_MASK
        if (offset == InterruptStruct.POP):
            return self._prev_pop
        if (offset == InterruptStruct.ACK):
            return self._ack
        if (self._fifo):
            number, argptr = self._fifo[0]
            if (offset == InterruptStruct.NUMBER):
                return number & WORD_MASK
            if (offset == InterruptStruct.ARGPTR_LO):
                return argptr & WORD_MASK
            if (offset == InterruptStruct.ARGPTR_HI):
                return (argptr >> 8) & WORD_MASK
        return 0

    def write(self, offset: int, value: int) -> None:
        if (offset != InterruptStruct.POP):
            return
        pop: int = value & WORD_MASK
        if (self._prev_pop == 0 and pop != 0):
            if (self._fifo):
                self._fifo.pop(0)
            self._ack = 1
        elif (self._prev_pop != 0 and pop == 0):
            self._ack = 0
        self._prev_pop = pop
