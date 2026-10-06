from enum import IntEnum

from constants import WORD_MASK, UART_BASE
from peripheral import Peripheral


class UartStruct(IntEnum):
    TX_READY: int = 0
    TX_DATA: int  = 1
    RX_VALID: int = 2
    RX_DATA: int  = 3


class Uart(Peripheral):

    def __init__(self, base: int = UART_BASE, baud: int = 9600, frame_bits: int = 10) -> None:
        self.base = base
        self.size = 4
        self._bit_time = 1_000_000_000 // baud
        self._frame_bits = frame_bits
        self.reset()


    def reset(self) -> None:
        self._tx = []
        self._rx = []
        self._tx_busy_until = 0


    def feed_rx(self, byte: int) -> None:
        self._rx.append(byte & WORD_MASK)


    def drain_tx(self) -> bytes:
        out: bytes = bytes(self._tx)
        self._tx = []
        return out


    def read(self, offset: int) -> int:
        if (offset == UartStruct.TX_READY):
            return 1 if self.clock.now() >= self._tx_busy_until else 0
        if (offset == UartStruct.RX_VALID):
            return 1 if self._rx else 0
        if (offset == UartStruct.RX_DATA):
            return self._rx.pop(0) if self._rx else 0
        return 0


    def write(self, offset: int, value: int) -> None:
        if (offset == UartStruct.TX_DATA):
            self._tx.append(value & WORD_MASK)
            self._tx_busy_until = self.clock.now() + self._frame_bits * self._bit_time
