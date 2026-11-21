#include "common/finishtest.h"

        .section text

Reset_Handler:
        SET2 RAB 0x0000
        SET2 RCD 0x0C00
        SET  RE  0x01
        SET  RF  0x00
        SET  RU  0xAA
        SET  RV  0x55

Memtest_Loop:
        // Test with 0xAA
        SET RF 0x00
        ST RU RA +0
        LD RF RA +0

        CMP RU RF
        JMPI EQUAL Memtest_Loop_0xAA_Good
Memtest_Loop_0xAA_Bad:
        Finish_Test(MEMTEST_FAILED_0xAA)
Memtest_Loop_0xAA_Good:

        // Test with 0x55
        SET RF 0x00
        ST RV RA +0
        LD RF RA +0

        CMP RV RF
        JMPI EQUAL Memtest_Loop_0x55_Good
Memtest_Loop_0x55_Bad:
        Finish_Test(MEMTEST_FAILED_0x55)
Memtest_Loop_0x55_Good:

        // Loop management
        ADD2 RAB RAB R0 RE
        SUB R0 RB RD
        SBB R0 RA RC
        JMPI BELOW Memtest_Loop
MemTest_Loop_End:
        
        Finish_Test(MEMTEST_SUCCESS)
