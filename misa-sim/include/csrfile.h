#ifndef _CSRFILE_H_
#define _CSRFILE_H_


#include "grammar.h"

#include <stdint.h>


struct csrfile {
    uint16_t c[FLAGS - SADDR + 1];
};


uint16_t csrfile_read(struct csrfile *csrfile, enum csrreg reg);


void csrfile_write(struct csrfile *csrfile, enum csrreg reg, uint16_t dword);


#endif  // _CSRFILE_H_
