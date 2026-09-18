        .section text
_start:
        set ra 0x01
        syscall ra
        set rb 0x5A
        halt rb

handler:
        rets

        .section vectors
        .space 10
        .addr handler
        .space 2
        .addr _start
