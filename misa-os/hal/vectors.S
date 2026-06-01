        .section vectors

        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .array 0x00 0x00  // Reserved
        .addr  mos_syscall_handler
        .addr  mos_fault_handler
        .addr  mos_reset_handler


        .section text

mos_reset_handler:
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
_mos_reset_handler_loop:
        goto _mos_reset_handler_loop


mos_fault_handler:
        halt r0


mos_syscall_handler:
        halt r0
