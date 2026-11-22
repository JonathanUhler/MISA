#macro FIRSTTEST_SUCCESS 0x00 #endm

#macro MEMTEST_SUCCESS     0x10 #endm
#macro MEMTEST_FAILED_0xAA 0x1A #endm
#macro MEMTEST_FAILED_0x55 0x15 #endm

#macro Finish_Test(code)
    SET RT code
    HALT RT
#endm
