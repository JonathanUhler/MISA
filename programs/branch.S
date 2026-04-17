        .section text

        set2 ryz label
        jmp always ryz

        .section vectors
label:
        set rt 0xFF
        halt rt
