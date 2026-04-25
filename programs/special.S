        set2 rab 0xAABB
        set2 rcd 0xCCDD
        set2 ref 0xEEFF
        set2 rgh 0x1122

        wsr saddr rab
        wsr raddr rcd
        wsr flags ref
        wsr cause rgh

        rsr saddr ruv
        rsr raddr ruv
        rsr flags ruv
        rsr cause ruv
