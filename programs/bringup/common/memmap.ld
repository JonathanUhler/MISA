section 0x0000 - 0x00FF : direct      ;  // Zero pages
section 0x0100 - 0X01FF :             ;  // Stack
section 0x0200 - 0x7FFF : data bss    ;  // RAM
section 0x8000 - 0xBFFF :             ;  // Memory-mapped IO
section 0xC000 - 0xFFEF : text rodata ;  // Program ROM
section 0xFFF0 - 0xFFFF : vectors     ;  // Vector table

// memory 0x0000 - 0xFFFF : rwx ;  // All memory available and mapped
