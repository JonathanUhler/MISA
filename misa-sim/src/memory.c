#include "memory.h"

#include <stddef.h>
#include <stdint.h>


uint8_t memory_read(struct memory *memory, uint16_t addr) {
    if (memory == NULL) {
        return 0;
    }
    return memory->m[addr];
}


void memory_write(struct memory *memory, uint16_t addr, uint8_t word) {
    if (memory == NULL) {
        return;
    }
    memory->m[addr] = word;
}
