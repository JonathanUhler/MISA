        .section text

        set2 ryz label
        jal always ryz

        set rt 0xFF
        halt rt

        .section vectors
label:
        ret
