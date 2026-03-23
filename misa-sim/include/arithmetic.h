#ifndef _ARITHMETIC_H_
#define _ARITHMETIC_H_


#include <cstdint>
#include <systemc.h>


SC_MODULE(Arithmetic) {
    sc_in<bool> alu_negb;
    sc_in<bool> alu_and;
    sc_in<bool> alu_or;
    sc_in<bool> alu_xor;
    sc_in<bool> alu_rot;
    sc_in<bool> alu_add;
    sc_in<bool> alu_cin;
    sc_in<uint8_t> alu_a;
    sc_in<uint8_t> alu_b;

    sc_out<bool> alu_z;
    sc_out<bool> alu_cout;
    sc_out<bool> alu_n;
    sc_out<bool> alu_v;
    sc_out<uint8_t> alu_y;


    SC_CTOR(Arithmetic) {
        SC_METHOD(execute);
        sensitive << alu_negb << alu_and << alu_or << alu_xor << alu_rot << alu_add << alu_cin
                  << alu_a << alu_b;
    }


    void execute();
};


#endif  // _ARITHMETIC_H_
