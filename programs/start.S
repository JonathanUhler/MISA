        .section vectors

        .array 0x00 0x00
        .array 0x00 0x00
        .array 0x00 0xC0


        .section text
_start:
        set2 ryz 0xABCD
        wsr saddr ryz

        set2 ryz main
        jal always ryz

        halt rt
