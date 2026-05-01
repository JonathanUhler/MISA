        #macro
        MY_MACRO 5
        #endm

        #macro
        MY_NOP
        add r0 r0 r0
        #endm


        MY_NOP
        set r1 MY_MACRO
        halt r1
