import os

from clock import VirtualClock
from uart import Uart, UartStruct
from constants import UART_BASE
from uart_console import UartConsole, open_raw_terminal, read_available


def _uart():
    u = Uart(UART_BASE, baud=1_000_000, frame_bits=10)
    u.attach(VirtualClock(), None)
    return u


def test_pump_feeds_rx_and_echoes_when_enabled():
    u = _uart()
    console = UartConsole(u, echo=True)
    assert console.pump(b"hi") == b"hi"          # echoed back to the caller
    assert u.read(UartStruct.RX_VALID) == 1
    assert u.read(UartStruct.RX_DATA) == ord("h")
    assert u.read(UartStruct.RX_DATA) == ord("i")


def test_pump_echo_off_feeds_rx_silently():
    u = _uart()
    console = UartConsole(u, echo=False)
    assert console.pump(b"x") == b""             # no echo
    assert u.read(UartStruct.RX_DATA) == ord("x")


def test_pump_returns_uart_tx_output():
    u = _uart()
    console = UartConsole(u, echo=False)
    u.write(UartStruct.TX_DATA, ord("A"))        # stands in for the program transmitting
    assert console.pump(b"") == b"A"


def test_read_available_returns_ready_bytes():
    r, w = os.pipe()
    os.write(w, b"abc")
    assert read_available(r) == b"abc"
    os.close(r)
    os.close(w)


def test_read_available_empty_when_nothing_ready():
    r, w = os.pipe()
    assert read_available(r) == b""
    os.close(r)
    os.close(w)


def test_open_raw_terminal_is_noop_for_non_tty():
    r, w = os.pipe()
    with open_raw_terminal(r):
        pass                          # must not raise on a non-TTY fd
    os.close(r)
    os.close(w)
