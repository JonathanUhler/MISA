#include "csrfile.h"
#include "grammar.h"

#include <stddef.h>
#include <stdint.h>


uint16_t csrfile_read(struct csrfile *csrfile, enum csrreg csr) {
    if (csrfile == NULL) {
        return 0;
    }
    return csrfile->c[(uint8_t) csr];
}


void csrfile_write(struct csrfile *csrfile, enum csrreg csr, uint16_t dword) {
    if (csrfile == NULL) {
        return;
    }
    csrfile->c[(uint8_t) csr] = dword;
}
