#include "csrfile.h"

#include <cstdint>


void Csrfile::execute() {
    bool wrsaddr = csr_wrsaddr.read();
    bool wrraddr = csr_wrraddr.read();
    bool wrflags = csr_wrflags.read();
    bool wrcause = csr_wrcause.read();

    if (wrsaddr) {
        csr_saddrout.write(csr_saddrin.read());
    }
    if (wrraddr) {
        csr_raddrout.write(csr_raddrin.read());
    }
    if (wrflags) {
        csr_flagsout.write(csr_flagsin.read());
    }
    if (wrcause) {
        csr_causeout.write(csr_causein.read());
    }
}
