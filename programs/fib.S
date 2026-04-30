        .section text

main:
        set rt 3

        set ra 0
        set rb 1

fib_loop:
        cmp rt r0
        jmpi equal fib_loop_end

        add rc ra rb
        mov ra rb
        mov rb rc

        set rd 1
        sub rt rt rd
        goto fib_loop
fib_loop_end:
        ret

        mov rt ra
        ret
