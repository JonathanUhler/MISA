        set2 rab 0xAABB
        set2 rcd 0xCCDD
        set2 ref 0xEEFF
        set2 ruv 0x1122
        set2 rwx 0x3344

        wsr saddr rab
        wsr raddr rcd
        wsr flags ref
        wsr cause ruv
        wsr extns rwx

        rsr saddr ryz
        rsr raddr ryz
        rsr flags ryz
        rsr cause ryz
        rsr extns ryz
