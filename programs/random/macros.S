        #macro
        MY_MACRO 5
        #endm

        #macro
        MY_NOP
        add r0 r0 r0
        #endm


        MY_NOP
        set rt MY_MACRO
        halt rt
