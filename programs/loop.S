        .section text

main:
        wsr saddr r0 r0
        set ra 5
loop:
        cmp ra r0
        jmpi equal loop_end

        set rb 1
        rsr saddr ryz
        add rz rz rb
        adc ry ry r0
        wsr saddr ryz

        sub ra ra rb
        goto loop
loop_end:
        set rt 0xFF
        ret
