        .section text
_start:
        clr                    // FLAGS = 0, so the carry in is clear
        set ra 0x03
        rrc rb ra              // rb = 0x01, C = 1, Z = 0
        halt rb

        .section vectors
        .space 14
        .addr _start
