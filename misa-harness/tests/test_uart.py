from clock import VirtualClock
from uart import Uart, UartStruct
from constants import UART_BASE


def _uart(clock):
    u = Uart(UART_BASE, baud=1_000_000, frame_bits=10)   # 1000 ns per bit
    u.attach(clock, None)
    return u


def test_tx_ready_initially():
    assert _uart(VirtualClock()).read(UartStruct.TX_READY) == 1


def test_tx_busy_until_frame_elapses():
    clock = VirtualClock()
    u = _uart(clock)
    u.write(UartStruct.TX_DATA, 0x41)
    assert u.read(UartStruct.TX_READY) == 0     # busy for 10 * 1000 ns
    clock.advance(9)                             # 9000 ns
    assert u.read(UartStruct.TX_READY) == 0
    clock.advance(1)                             # 10000 ns, frame complete
    assert u.read(UartStruct.TX_READY) == 1
    assert u.drain_tx() == b"A"


def test_rx_read_pops_a_byte():
    u = _uart(VirtualClock())
    assert u.read(UartStruct.RX_VALID) == 0
    u.feed_rx(0x42)
    assert u.read(UartStruct.RX_VALID) == 1
    assert u.read(UartStruct.RX_DATA) == 0x42
    assert u.read(UartStruct.RX_VALID) == 0
