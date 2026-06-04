        .section data

#macro ARRAY_LEN 10 #endm
array_ptr:      
        .array 8 72 59 17 65 5 104 74 179 46


        .section text

arraysum:
        push2 ruv
        push2 rwx
        push2 ryz

        mov2 ruv rab
        mov  rw  rc
        set  rx  1
        set2 ryz 0

_arraysum_loop:
        cmp rw r0
        jmpi equal _arraysum_loop_end

        ld rt ruv
        add2 ryz ryz r0 rt

        add2 ruv ruv r0 rx
        sub rw rw rx
        goto _arraysum_loop
_arraysum_loop_end:

        mov2 rab ryz

        pop2 ryz
        pop2 rwx
        pop2 ruv
        ret


main:
        rsr raddr ruv
        push2 ruv

        set2 rab array_ptr
        set  rc  ARRAY_LEN
        call arraysum

        pop2 ruv
        wsr raddr ruv
        ret
