#ifndef _MEMORY_H_
#define _MEMORY_H_


#include <cstdint>
#include <systemc.h>


SC_MODULE(Memory) {
    uint8_t m[UINT16_MAX];

    sc_in<bool> mem_read;
    sc_in<bool> mem_write;
    sc_in<uint8_t> mem_wdata;
    sc_in<uint16_t> mem_addr;

    sc_out<bool> mem_ready;
    sc_out<uint8_t> mem_rdata;


    SC_CTOR(Memory) {
        SC_METHOD(execute);
        sensitive << mem_wdata << mem_addr << mem_read << mem_write;
    }


    void execute();
};


#endif  // _MEMORY_H_
