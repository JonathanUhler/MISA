        .section text

        set2 rscratch label
        jal always rscratch

        set rt 0xFF
        halt rt

        .section vectors
label:
        ret
