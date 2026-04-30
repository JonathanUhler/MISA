        .section text
_start:
        set2 rscratch 0xABCD
        wsr saddr rscratch

        set2 rscratch main
        jal always rscratch

        halt rt
