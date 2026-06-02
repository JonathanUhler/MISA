        .section vectors

        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .addr  hal_syscall_handler
        .addr  hal_fault_handler
        .addr  hal_reset_handler


        .section text

hal_reset_handler:
        set2 rab 0x0000
        set2 rcd 0x0000
        set2 ref 0x0000
        set2 ruv 0x0000
        set2 rwx 0x0000
        set2 ryz 0x0000
        set  rt  0x00

        wsr saddr rab
        wsr raddr rab
        wsr flags rab
        wsr cause rab
        wsr retsc rab
        wsr privs rab

        goto _start
_hal_reset_handler_loop:
        goto _hal_reset_handler_loop


hal_fault_handler:
        halt r0


hal_syscall_handler:
        halt r0
