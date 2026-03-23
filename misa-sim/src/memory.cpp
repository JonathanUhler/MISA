#include "memory.h"

#include <cstdint>


void Memory::execute() {
    bool read = mem_read.read();
    bool write = mem_write.read();

    if (read) {
        uint8_t data = m[mem_addr.read()];
        mem_rdata.write(data);
        mem_ready.write(true);
    }
    if (write) {
        uint8_t data = mem_wdata.read();
        m[mem_addr.read()] = data;
        mem_ready.write(true);
    }

    if (!read && !write) {
        mem_ready.write(false);
    }
}
