#ifndef _COMPARATOR_H_
#define _COMPARATOR_H_


#include "grammar.h"

#include <cstdint>
#include <systemc.h>


SC_MODULE(Comparator) {
    sc_in<uint16_t> cmp_flags;

    sc_out<bool> cmp_always;
    sc_out<bool> cmp_equal;
    sc_out<bool> cmp_notequal;
    sc_out<bool> cmp_greater;
    sc_out<bool> cmp_less;
    sc_out<bool> cmp_greaterequal;
    sc_out<bool> cmp_lessequal;


    SC_CTOR(Comparator) {
        SC_METHOD(execute);
        sensitive << cmp_flags;
    }


    void execute();
};


#endif  // _COMPARATOR_H_
