#include "grammar.h"
#include "regfile.h"

#include <stddef.h>
#include <stdint.h>


uint8_t regfile_read(struct regfile *regfile, enum gpreg reg) {
    if (regfile == NULL) {
        return 0;
    }
    return regfile->r[(uint8_t) reg];
}


void regfile_write(struct regfile *regfile, enum gpreg reg, uint8_t word) {
    if (regfile == NULL) {
        return;
    }
    regfile->r[(uint8_t) reg] = word;
}
