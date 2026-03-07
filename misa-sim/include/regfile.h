#ifndef _REGFILE_H_
#define _REGFILE_H_


#include "grammar.h"

#include <stdint.h>


struct regfile {
    uint8_t r[RT - R0 + 1];
};


uint8_t regfile_read(struct regfile *regfile, enum gpreg reg);


void regfile_write(struct regfile *regfile, enum gpreg reg, uint8_t word);


#endif  // _REGFILE_H_
