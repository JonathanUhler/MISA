section 0x0000 - 0x00FF : direct ;       // Zero page
section 0x0100 - 0X01FF : ;              // Stack
section 0x0200 - 0x7FFF : data bss ;     // RAM
section 0x8000 - 0xBFFF : ;              // Memory-mapped IO
section 0xC000 - 0xFFF0 : text rodata ;  // Program ROM
section 0xFFFA - 0xFFFF : vectors ;      // Hardware vector addresses
