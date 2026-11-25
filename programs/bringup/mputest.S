#include "common/bringup.h"
#include "common/finishtest.h"

#macro MPU_TEXT_X   0x21 #endm
#macro MPU_DATA_RW  0x0E #endm
#macro MPU_DATA_R   0x0C #endm
#macro MPU_DATA_RWX 0x17 #endm

#macro MPU_PROBE 0x355C #endm

#macro PRIV_ACCESS      0x12 #endm
#macro PRIV_INSTRUCTION 0x11 #endm
#macro ACCESS_EXECUTE   0x01 #endm
#macro ACCESS_WRITE     0x02 #endm
#macro ACCESS_NONE      0x00 #endm

#macro MPU_PATTERN 0x5A #endm


        .section data

Mpu_Cell:
        .word 0x00


        .section text

Reset_Handler:
        SET2 RSCRATCH STACK_TOP
        WSR SADDR RSCRATCH

        SET RZ 0x00
        RSR MPUCF RUV
        CMP RU R0
        JMPI NOT_EQUAL Mputest_Failed_Reset
        CMP RV R0
        JMPI NOT_EQUAL Mputest_Failed_Reset

        SET2 RAB MPU_PROBE
        WSR MPUCF RAB
        RSR MPUCF RUV
        CMP RU RA
        JMPI NOT_EQUAL Mputest_Failed_Readback
        CMP RV RB
        JMPI NOT_EQUAL Mputest_Failed_Readback

        SET RZ 0x01
        SET2 RAB USER_MODE
        WSR PRIVS RAB
Mputest_Stage_1_User:
        SET RT MPUTEST_FAILED_HALT
        HALT RT

Mputest_Stage_2:
        SET RZ 0x02
        SET2 RAB Mputest_Stage_2_User
        SET RB MPU_TEXT_X
        WSR MPUCF RAB
        SET2 RAB USER_MODE
        WSR PRIVS RAB
Mputest_Stage_2_User:
        SET RC MPU_PATTERN
        SET2 RAB Mpu_Cell
        ST RC RAB +0
        SET RT MPUTEST_FAILED_HALT
        HALT RT

Mputest_Stage_3:
        SET RZ 0x03
        SET2 RAB Mpu_Cell
        SET RB MPU_DATA_RW
        WSR MPUCF RAB
        SET2 RAB USER_MODE
        WSR PRIVS RAB
Mputest_Stage_3_User:
        SET2 RAB Mpu_Cell
        LD RE RAB +0
        SET RC MPU_PATTERN
        ST RC RAB +0
        LD RF RAB +0
        SET RT MPUTEST_FAILED_HALT
        HALT RT

Mputest_Stage_4:
        SET RZ 0x04
        SET2 RAB Mpu_Cell
        SET RB MPU_DATA_RWX
        WSR MPUCF RAB
        SET2 RAB Mpu_Cell
        SET RB MPU_DATA_R
        WSR MPUCF RAB
        SET2 RAB USER_MODE
        WSR PRIVS RAB
Mputest_Stage_4_User:
        SET2 RAB Mpu_Cell
        LD RE RAB +0
        SET RC 0x00
        ST RC RAB +0
        SET RT MPUTEST_FAILED_INDEX
        HALT RT


Fault_Handler:
        SET RSCRATCH0 0x01
        CMP RZ RSCRATCH0
        JMPI EQUAL Fault_Stage_1
        SET RSCRATCH0 0x02
        CMP RZ RSCRATCH0
        JMPI EQUAL Fault_Stage_2
        SET RSCRATCH0 0x03
        CMP RZ RSCRATCH0
        JMPI EQUAL Fault_Stage_3
        SET RSCRATCH0 0x04
        CMP RZ RSCRATCH0
        JMPI EQUAL Fault_Stage_4

        Finish_Fault

Fault_Stage_1:
        RSR CAUSE RUV
        SET RW PRIV_ACCESS
        CMP RV RW
        JMPI NOT_EQUAL Mputest_Failed_Fetch
        SET RW ACCESS_EXECUTE
        CMP RU RW
        JMPI NOT_EQUAL Mputest_Failed_Fetch
        GOTO Mputest_Stage_2

Fault_Stage_2:
        RSR CAUSE RUV
        SET RW PRIV_ACCESS
        CMP RV RW
        JMPI NOT_EQUAL Mputest_Failed_Write
        SET RW ACCESS_WRITE
        CMP RU RW
        JMPI NOT_EQUAL Mputest_Failed_Write
        GOTO Mputest_Stage_3

Fault_Stage_3:
        RSR CAUSE RUV
        SET RW PRIV_INSTRUCTION
        CMP RV RW
        JMPI NOT_EQUAL Mputest_Failed_Data
        SET RW ACCESS_NONE
        CMP RU RW
        JMPI NOT_EQUAL Mputest_Failed_Data
        CMP RE R0
        JMPI NOT_EQUAL Mputest_Failed_Data
        SET RW MPU_PATTERN
        CMP RF RW
        JMPI NOT_EQUAL Mputest_Failed_Data
        GOTO Mputest_Stage_4

Fault_Stage_4:
        RSR CAUSE RUV
        SET RW PRIV_ACCESS
        CMP RV RW
        JMPI NOT_EQUAL Mputest_Failed_Index
        SET RW ACCESS_WRITE
        CMP RU RW
        JMPI NOT_EQUAL Mputest_Failed_Index
        SET RW MPU_PATTERN
        CMP RE RW
        JMPI NOT_EQUAL Mputest_Failed_Data
        Finish_Test(MPUTEST_SUCCESS)


Mputest_Failed_Reset:
        Finish_Test(MPUTEST_FAILED_RESET)

Mputest_Failed_Readback:
        Finish_Test(MPUTEST_FAILED_READBACK)

Mputest_Failed_Fetch:
        Finish_Test(MPUTEST_FAILED_FETCH)

Mputest_Failed_Write:
        Finish_Test(MPUTEST_FAILED_WRITE)

Mputest_Failed_Data:
        Finish_Test(MPUTEST_FAILED_DATA)

Mputest_Failed_Index:
        Finish_Test(MPUTEST_FAILED_INDEX)


Syscall_Handler:
        Finish_Test(UNEXPECTED_SYSCALL)


Interrupt_Handler:
        Finish_Test(UNEXPECTED_INTERRUPT)
