// UART echo demo for the MISA architecture.
//
// Reads characters from the UART into a line buffer and echoes the whole line back when Enter
// (0x0A) is pressed. An empty line quits with HALT. Bytes beyond the buffer size are dropped until
// the next Enter. This program only polls the UART, it uses no interrupts and no stack.
//
// UART registers (UART_BASE = 0x8010, see misa-harness/uart.py):
//   0x8010  TX_READY  read   1 when the transmitter can accept a byte
//   0x8011  TX_DATA   write  byte to transmit
//   0x8012  RX_VALID  read   1 when a received byte is waiting
//   0x8013  RX_DATA   read   next received byte (reading pops it)
//
// Register usage:
//   r0      constant zero
//   ra      scratch: status reads and the current byte
//   rb      line length (bytes buffered)
//   rc:rd   16-bit buffer pointer (rc high, rd low)
//   re:rf   scratch: MMIO addresses and small constants
//   rt      halt code

        .section text
_start:
receive_line:
        set2 rc rd buffer           // rc:rd = write pointer at the buffer start
        set rb 0x00                 // line length = 0

rx_wait:
        set2 re rf 0x8012           // RX_VALID
        ld ra re rf
        cmp ra r0
        jmpi equal rx_wait          // spin until a byte arrives

        set2 re rf 0x8013           // RX_DATA
        ld ra re rf                 // ra = received byte

        set re 0x0A                 // Enter ends the line
        cmp ra re
        jmpi equal transmit_line

        set re 0x40                 // buffer holds at most 64 bytes
        cmp rb re
        jmpi equal rx_wait          // full, drop the byte and keep reading

        st ra rc rd                 // buffer[ptr] = byte
        set rf 0x01
        add rd rd rf                // pointer low += 1 (sets carry)
        adc rc rc r0                // pointer high += carry
        add rb rb rf                // length += 1
        goto rx_wait

transmit_line:
        cmp rb r0
        jmpi equal do_quit          // empty line, quit
        set2 rc rd buffer           // rc:rd = read pointer at the buffer start

tx_loop:
        cmp rb r0
        jmpi equal tx_newline       // whole line sent

tx_wait_ready:
        set2 re rf 0x8010           // TX_READY
        ld ra re rf
        cmp ra r0
        jmpi equal tx_wait_ready    // spin until the transmitter is ready

        ld ra rc rd                 // ra = buffer[ptr]
        set2 re rf 0x8011           // TX_DATA
        st ra re rf                 // transmit the byte

        set rf 0x01
        add rd rd rf                // pointer low += 1
        adc rc rc r0                // pointer high += carry
        sub rb rb rf                // length -= 1
        goto tx_loop

tx_newline:
        set2 re rf 0x8010           // TX_READY
        ld ra re rf
        cmp ra r0
        jmpi equal tx_newline       // spin until the transmitter is ready
        set ra 0x0A                 // append a newline so the line stands alone
        set2 re rf 0x8011           // TX_DATA
        st ra re rf
        goto receive_line

do_quit:
        set rt 0x00
        halt rt

        .section data
buffer:
        .space 64                   // 64-byte line buffer (matches the 0x40 limit above)

        .section vectors
        .space 14
        .addr _start                // reset vector at 0xFFFE points here
