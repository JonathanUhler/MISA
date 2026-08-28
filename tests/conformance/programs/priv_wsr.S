        .section text
_start:
        set ra 0x00
        set rb 0x01
        wsr privs ra rb        // enter User Mode, ring bit set
        wsr privs r0 r0        // write PRIVS in User Mode, must fault
        set re 0xAA            // sentinel, runs only if the write did not fault
        halt re

__fault:
        halt r0

        .section vectors
        .space 12
        .addr __fault
        .addr _start
