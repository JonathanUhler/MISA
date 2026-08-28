        .section text
_start:
        set ra 5
        set rb 3
        add rc ra rb
        halt rc

        .section vectors
        .space 14
        .addr _start
