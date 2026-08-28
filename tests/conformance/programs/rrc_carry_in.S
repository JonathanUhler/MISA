        .section text
_start:
        set ra 0xFF
        set rb 0x01
        add rc ra rb           // 0xFF + 0x01 overflows, setting C = 1
        set rd 0x00
        rrc re rd              // carry in shifts into bit 7, re = 0x80, N = 1
        halt re

        .section vectors
        .space 14
        .addr _start
