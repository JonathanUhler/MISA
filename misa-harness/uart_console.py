import contextlib
import os
import select
import termios


class UartConsole:

    def __init__(self, uart, echo: bool = True) -> None:
        self._uart = uart
        self._echo = echo


    def pump(self, incoming: bytes) -> bytes:
        out = bytearray()
        for b in incoming:
            self._uart.feed_rx(b)
            if (self._echo):
                out.append(b)
        out += self._uart.drain_tx()
        return bytes(out)


def read_available(fd, max_bytes: int = 4096) -> bytes:
    ready, _, _ = select.select([fd], [], [], 0)
    if (not ready):
        return b""
    try:
        return os.read(fd, max_bytes)
    except OSError:
        return b""


@contextlib.contextmanager
def open_raw_terminal(fd):
    if (not os.isatty(fd)):
        yield
        return
    saved = termios.tcgetattr(fd)
    try:
        mode = termios.tcgetattr(fd)
        mode[3] &= ~(termios.ICANON | termios.ECHO)   # lflag: char-at-a-time, no local echo
        termios.tcsetattr(fd, termios.TCSADRAIN, mode)
        yield
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, saved)
