        .section text

        set2 rscratch label
        jmp always rscratch

        .section vectors
label:
        set rt 0xFF
        halt rt
