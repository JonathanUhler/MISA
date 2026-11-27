// Finish codes and reporters for the bringup programs.

#macro FAULT_TAG 0x80 #endm

#macro UNEXPECTED_INTERRUPT 0xF0 #endm
#macro UNEXPECTED_SYSCALL   0xF1 #endm

#macro FIRSTTEST_SUCCESS 0x00 #endm

#macro MEMTEST_SUCCESS     0x10 #endm
#macro MEMTEST_FAILED_0xAA 0x1A #endm
#macro MEMTEST_FAILED_0x55 0x15 #endm

#macro SYNCTEST_SUCCESS      0x20 #endm
#macro SYNCTEST_FAILED_SUM   0x21 #endm
#macro SYNCTEST_FAILED_SAVED 0x22 #endm
#macro SYNCTEST_FAILED_RETSC 0x23 #endm
#macro SYNCTEST_FAILED_CODE  0x24 #endm
#macro SYNCTEST_FAILED_CALL  0x25 #endm

#macro MPUTEST_SUCCESS         0x30 #endm
#macro MPUTEST_FAILED_RESET    0x31 #endm
#macro MPUTEST_FAILED_READBACK 0x32 #endm
#macro MPUTEST_FAILED_FETCH    0x33 #endm
#macro MPUTEST_FAILED_WRITE    0x34 #endm
#macro MPUTEST_FAILED_DATA     0x35 #endm
#macro MPUTEST_FAILED_INDEX    0x36 #endm
#macro MPUTEST_FAILED_HALT     0x37 #endm

#macro INTTEST_SUCCESS        0x40 #endm
#macro INTTEST_NO_INTERRUPT   0x41 #endm
#macro INTTEST_FAILED_SCRATCH 0x42 #endm
#macro INTTEST_FAILED_FLAGS   0x43 #endm
#macro INTTEST_FAILED_COUNT   0x44 #endm

// A bringup program ends by invoking Finish_Test with one of the finish codes above. Finish_Test
// halts the processor with that code using the RT register.

#macro Finish_Test(code)
    SET RT code
    HALT RT
#endm

// Finish_Fault is for the fault handler of a program which expects no fault. It transfers the
// fault Reason and Type to a register and then halts with that register, tagged so the campaign
// can tell a reported fault from a finish code.

#macro Finish_Fault
    RSR CAUSE RSCRATCH
    SET RSCRATCH0 FAULT_TAG
    OR  RSCRATCH1 RSCRATCH1 RSCRATCH0
    HALT RSCRATCH1
#endm
