// Reserved to ensure null pointer accesses don't overwrite important .direct data
section 0x0000 - 0x0001 : null_trap   ;

// Unreserved portion of direct access section, which may be faster on some hardware
section 0x0002 - 0x0037 : direct      ;

// TCB array in direct access memory
section 0x0038 - 0x0050 : tcb_idle    ;
section 0x0051 - 0x0069 : tcb_1       ;
section 0x006A - 0x0082 : tcb_2       ;
section 0x0083 - 0x009B : tcb_3       ;
section 0x009C - 0x00B4 : tcb_4       ;
section 0x00B5 - 0x00CD : tcb_5       ;
section 0x00CE - 0x00E6 : tcb_6       ;
section 0x00E7 - 0x00FF : tcb_7       ;

// Kernel stack memory
section 0x0100 - 0X01FF : kernel_stack ;

// Program data
section 0x0200 - 0x7FFF : data bss    ;

// RAM and MMIO
section 0x8000 - 0xB8FF : ram mmio    ;

// Task stacks
section 0xB900 - 0xB9FF : stack_1     ;
section 0xBA00 - 0xBAFF : stack_2     ;
section 0xBB00 - 0xBBFF : stack_3     ;
section 0xBC00 - 0xBCFF : stack_4     ;
section 0xBD00 - 0xBDFF : stack_5     ;
section 0xBE00 - 0xBEFF : stack_6     ;
section 0xBF00 - 0xBFFF : stack_7     ;

// Program text
section 0xC000 - 0xFFEF : text rodata ;

// Vector table
section 0xFFF0 - 0xFFFF : vectors     ;
