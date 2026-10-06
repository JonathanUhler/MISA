from abc import ABC, abstractmethod

from clock import Clock


class Peripheral(ABC):

    base: int = 0
    size: int = 0


    def attach(self, clock: Clock, irq) -> None:
        self.clock = clock
        self.irq = irq


    @abstractmethod
    def read(self, offset: int) -> int:
        """Return the byte at a base-relative offset. May have read side effects."""


    @abstractmethod
    def write(self, offset: int, value: int) -> None:
        """Write a byte to a base-relative offset."""


    def poll(self) -> None:
        """Advance time-based behavior. Default is a no-op."""


    def reset(self) -> None:
        """Return to the power-on state. Default is a no-op."""


def pack_le(value: int, nbytes: int) -> list:
    return [(value >> (8 * i)) & 0xFF for i in range(nbytes)]


def unpack_le(byte_list) -> int:
    return sum(b << (8 * i) for i, b in enumerate(byte_list))
