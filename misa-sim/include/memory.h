#ifndef _MEMORY_H_
#define _MEMORY_H_


#include <stdint.h>


struct memory {
    uint8_t m[65535];
};


uint8_t memory_read(struct memory *memory, uint16_t addr);


void memory_write(struct memory *memory, uint16_t addr, uint8_t word);


#endif  // _MEMORY_H_
