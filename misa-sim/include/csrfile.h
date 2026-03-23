#ifndef _CSRFILE_H_
#define _CSRFILE_H_


#include "grammar.h"

#include <cstdint>
#include <systemc.h>


SC_MODULE(Csrfile) {
    sc_in<bool> csr_wrsaddr;
    sc_in<bool> csr_wrraddr;
    sc_in<bool> csr_wrflags;
    sc_in<bool> csr_wrcause;
    sc_in<uint16_t> csr_saddrin;
    sc_in<uint16_t> csr_raddrin;
    sc_in<uint16_t> csr_flagsin;
    sc_in<uint16_t> csr_causein;

    sc_out<uint16_t> csr_saddrout;
    sc_out<uint16_t> csr_raddrout;
    sc_out<uint16_t> csr_flagsout;
    sc_out<uint16_t> csr_causeout;


    SC_CTOR(Csrfile) {
        SC_METHOD(execute);
        sensitive << csr_wrsaddr << csr_wrraddr << csr_wrflags << csr_wrcause
                  << csr_saddrin << csr_raddrin << csr_flagsin << csr_causein;
    }


    void execute();
};


#endif  // _CSRFILE_H_
