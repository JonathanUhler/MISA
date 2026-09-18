import time
from abc import ABC, abstractmethod


class Clock(ABC):

    @abstractmethod
    def now(self) -> int:
        """Monotonic time in nanoseconds."""


class VirtualClock(Clock):

    def __init__(self, hz: int = 1_000_000) -> None:
        self._t = 0
        self._period = 1_000_000_000 // hz


    def now(self) -> int:
        return self._t


    def advance(self, cycles: int = 1) -> None:
        self._t += cycles * self._period


    def reset(self) -> None:
        self._t = 0


class RealClock(Clock):

    def now(self) -> int:
        return time.monotonic_ns()
