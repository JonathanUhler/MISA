        .section reset

reset:
        set2 ryz _start
        jmp always ryz
        halt r0
