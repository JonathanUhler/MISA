        .section vectors

        .array 0x00 0x00
        .array 0x00 0x00
        .array 0x00 0x00
        .array 0x00 0x00
        .array 0x00 0x00
        .addr Syscall_Handler
        .array 0x00 0x00
        .addr Reset_Handler


        .section text

Reset_Handler:
        set2 rscratch 0x01FF
        wsr saddr rscratch
        call main
        halt rt


Syscall_Handler:
        push ru
        push rv

        rsr cause ru r0

        set rv 0xAD
        cmp ru rv
        jmpi not_equal _Syscall_Handler_end
_Syscall_Handler_AD:
        add rt ra rb
_Syscall_Handler_end:

        pop rv
        pop ru
        rets


main:
        set ra 0x12
        set rb 0x34

        set rt 0xAD
        syscall rt

        set rc 0x46
        cmp rt rc
        jmpi equal _main_good

_main_bad:
        set rt 0x01
        ret
_main_good:
        set rt 0x00
        ret
