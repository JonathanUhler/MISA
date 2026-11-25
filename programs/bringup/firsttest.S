#include "common/finishtest.h"

        .section text

Reset_Handler:
        Finish_Test(FIRSTTEST_SUCCESS)


Fault_Handler:
        Finish_Fault


Syscall_Handler:
        Finish_Test(UNEXPECTED_SYSCALL)


Interrupt_Handler:
        Finish_Test(UNEXPECTED_INTERRUPT)
