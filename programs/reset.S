        .section reset

reset:
        set2 rscratch _start
        jmp always rscratch
        halt r0
