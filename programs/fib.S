        .section text

fib:
        // ra = first value
        // rb = second value
        // rt = loop, counting down

        set ra 0
        set rb 1

fib_loop:
        set2 rscratch fib_loop_end
        cmp rt r0
        jmp equal rscratch

        add rc ra rb
        mov ra rb
        mov rb rc

        set rd 1
        sub rt rt rd
        set2 rscratch fib_loop
        jmp always rscratch
fib_loop_end:
        ret

main:
        set rt 5
        set2 rscratch fib
        jal always rscratch

        mov rt ra
        ret
