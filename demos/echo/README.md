# UART Echo Demo

A small MISA assembly program that turns the terminal into a serial console. It reads characters
from the UART into a 64-byte line buffer and echoes the whole line back when Enter is pressed. An
empty line quits. The program polls the UART only. It uses no interrupts and no stack.

## Build

Install the toolchain at the repository root, then build the demo.

```
make install     # run once from the repo root, builds and installs misa-as and misa-run
make             # from demos/echo, produces echo.bin
```

## Run

```
make run         # runs echo.bin under misa-run
```

The terminal becomes the UART console. Type a line and press Enter to see it echoed. Press Enter on
an empty line to quit. Ctrl-C also quits.

Non-interactive use pipes input straight through, which is handy for scripting and tests.

```
printf 'hello\nworld\n\n' | misa-run echo.bin
```

## How It Works

The program polls the UART registers at `UART_BASE = 0x8010`.

| Address  | Register | Access | Meaning                                  |
|----------|----------|--------|------------------------------------------|
| `0x8010` | TX_READY | read   | 1 when the transmitter can accept a byte |
| `0x8011` | TX_DATA  | write  | byte to transmit                         |
| `0x8012` | RX_VALID | read   | 1 when a received byte is waiting        |
| `0x8013` | RX_DATA  | read   | next received byte, reading pops it      |

Received bytes are buffered until Enter (`0x0A`). The program then transmits the buffer one byte at
a time, waiting on `TX_READY` between bytes, and appends a newline so each echoed line stands alone.
Bytes past the 64-byte limit are dropped until the next Enter.
