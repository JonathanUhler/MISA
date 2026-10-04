        .section text
_start:
        set ra 0x02
        set rb 0x00
        set rc 0x2A
        st rc ra 0
        set rd 0x00
        ld rd ra 0
        halt rd

        .section vectors
        .space 14
        .addr _start
