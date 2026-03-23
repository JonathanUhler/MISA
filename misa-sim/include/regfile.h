#ifndef _REGFILE_H_
#define _REGFILE_H_


#include "grammar.h"

#include <cstdint>
#include <systemc.h>



SC_MODULE(Regfile) {
    uint8_t r[GPREG_MAX];

    sc_in<bool> reg_port1_write;
    sc_in<bool> reg_port2_write;
    sc_in<enum gpreg> reg_port1;
    sc_in<enum gpreg> reg_port2;
    sc_in<enum gpreg> reg_port3;

    sc_inout<uint8_t> reg_port1_data;
    sc_inout<uint8_t> reg_port2_data;

    sc_out<uint8_t> reg_port3_data;


    SC_CTOR(Regfile) {
        SC_METHOD(execute);
        sensitive << reg_port1 << reg_port2 << reg_port3;
    }


    void write_reg(enum gpreg reg, uint8_t value);


    uint8_t read_reg(enum gpreg reg);


    void execute();
};


#endif  // _REGFILE_H_
