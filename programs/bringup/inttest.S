#include "common/bringup.h"
#include "common/finishtest.h"


#macro INTTEST_ROUNDS      0x08 #endm
#macro INTTEST_SCRATCH_HI  0xC3 #endm
#macro INTTEST_SCRATCH_LO  0x3C #endm
#macro INTTEST_LEFT        0x12 #endm
#macro INTTEST_RIGHT       0x34 #endm


        .section data

Int_Count:
        .word 0x00


        .section text

Reset_Handler:
        SET2 RSCRATCH STACK_TOP
        WSR SADDR RSCRATCH

        SET2 RAB Int_Count
        ST R0 RAB +0

        SET RZ INTTEST_ROUNDS

Inttest_Loop:
        SET RSCRATCH0 INTTEST_SCRATCH_HI
        SET RSCRATCH1 INTTEST_SCRATCH_LO
        SET RC INTTEST_LEFT
        SET RD INTTEST_RIGHT
        CMP RC RD
        RSR FLAGS RUV

        // Expect an interrupt somewhere here
        NOP
        NOP
        NOP
        NOP

        RSR FLAGS RWX
        MOV RE RSCRATCH0
        MOV RF RSCRATCH1

        CMP RV RX
        JMPI NOT_EQUAL Inttest_Failed_Flags

        SET RC INTTEST_SCRATCH_HI
        CMP RE RC
        JMPI NOT_EQUAL Inttest_Failed_Scratch
        SET RC INTTEST_SCRATCH_LO
        CMP RF RC
        JMPI NOT_EQUAL Inttest_Failed_Scratch

        SET2 RAB Int_Count
        LD RC RAB +0
        CMP RC R0
        JMPI NOT_EQUAL Inttest_Took_Interrupt

        SET RC 0x01
        SUB RZ RZ RC
        JMPI NOT_EQUAL Inttest_Loop

        SET2 RAB Int_Count
        LD RC RAB +0
        CMP RC R0
        JMPI NOT_EQUAL Inttest_Took_Interrupt

        Finish_Test(INTTEST_NO_INTERRUPT)

Inttest_Took_Interrupt:
        SET RD 0x01
        CMP RD RC
        JMPI BELOW Inttest_Failed_Count
        Finish_Test(INTTEST_SUCCESS)

Inttest_Failed_Flags:
        Finish_Test(INTTEST_FAILED_FLAGS)

Inttest_Failed_Scratch:
        Finish_Test(INTTEST_FAILED_SCRATCH)

Inttest_Failed_Count:
        Finish_Test(INTTEST_FAILED_COUNT)


Interrupt_Handler:
        PUSH2 RAB
        RSR RADDR RAB
        PUSH2 RAB

        SET2 RAB Int_Count
        LD RSCRATCH0 RAB +0
        SET RSCRATCH1 0x01
        ADD RSCRATCH0 RSCRATCH0 RSCRATCH1
        ST RSCRATCH0 RAB +0

        POP2 RAB
        WSR RADDR RAB
        POP2 RAB
        RETI


Fault_Handler:
        Finish_Fault


Syscall_Handler:
        Finish_Test(UNEXPECTED_SYSCALL)
