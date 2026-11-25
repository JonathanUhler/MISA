#include "common/bringup.h"
#include "common/finishtest.h"


#macro SYNCTEST_CALL_ADD  0xA1 #endm
#macro SYNCTEST_CALL_CODE 0xA2 #endm

        .section text

Reset_Handler:
        SET2 RSCRATCH STACK_TOP
        WSR SADDR RSCRATCH

        SET RA 0x12
        SET RB 0x34
        SET RU 0x5A
        SET RV 0xA5

        SET RT SYNCTEST_CALL_ADD
        SYSCALL RT
Synctest_Add_Return:

        SET RC 0x46
        CMP RT RC
        JMPI NOT_EQUAL Synctest_Failed_Sum

        SET RC 0x5A
        CMP RU RC
        JMPI NOT_EQUAL Synctest_Failed_Saved
        SET RC 0xA5
        CMP RV RC
        JMPI NOT_EQUAL Synctest_Failed_Saved

        RSR RETSC RWX
        SET2 RYZ Synctest_Add_Return
        CMP RW RY
        JMPI NOT_EQUAL Synctest_Failed_Retsc
        CMP RX RZ
        JMPI NOT_EQUAL Synctest_Failed_Retsc

        SET RT SYNCTEST_CALL_CODE
        SYSCALL RT

        SET RC SYNCTEST_CALL_CODE
        CMP RT RC
        JMPI NOT_EQUAL Synctest_Failed_Code

        Finish_Test(SYNCTEST_SUCCESS)

Synctest_Failed_Sum:
        Finish_Test(SYNCTEST_FAILED_SUM)

Synctest_Failed_Saved:
        Finish_Test(SYNCTEST_FAILED_SAVED)

Synctest_Failed_Retsc:
        Finish_Test(SYNCTEST_FAILED_RETSC)

Synctest_Failed_Code:
        Finish_Test(SYNCTEST_FAILED_CODE)


Syscall_Handler:
        PUSH RU
        PUSH RV

        RSR CAUSE RUV

        SET RV SYNCTEST_CALL_ADD
        CMP RU RV
        JMPI EQUAL Syscall_Handler_Add

        SET RV SYNCTEST_CALL_CODE
        CMP RU RV
        JMPI EQUAL Syscall_Handler_Code

        Finish_Test(SYNCTEST_FAILED_CALL)

Syscall_Handler_Add:
        ADD RT RA RB
        GOTO Syscall_Handler_Return

Syscall_Handler_Code:
        MOV RT RU

Syscall_Handler_Return:
        POP RV
        POP RU
        RETS


Fault_Handler:
        Finish_Fault


Interrupt_Handler:
        Finish_Test(UNEXPECTED_INTERRUPT)
