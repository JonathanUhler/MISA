        .section text
_start:
        call set_answer
        halt ra

set_answer:
        set ra 0x63
        ret

        .section vectors
        .space 14
        .addr _start
