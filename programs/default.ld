SECTION 0x0000 - 0x00FF : direct ;       // Zero page
SECTION 0x0100 - 0X01FF : ;              // Stack
SECTION 0x0200 - 0x7FFF : data bss ;     // RAM
SECTION 0x8000 - 0xBFFF : ;              // Memory-mapped IO
SECTION 0xC000 - 0xFFF0 : text rodata ;  // Program ROM
SECTION 0xFFFA - 0xFFFF : vectors ;      // Hardware vector addresses
