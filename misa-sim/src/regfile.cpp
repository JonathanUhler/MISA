#include "grammar.h"
#include "regfile.h"

#include <cstdint>


void Regfile::write_reg(enum gpreg reg, uint8_t value) {
    if (reg >= GPREG_MAX || reg == R0) {
        return;
    }
    r[reg] = value;
}


uint8_t Regfile::read_reg(enum gpreg reg) {
    if (reg >= GPREG_MAX || reg == R0) {
        return 0;
    }
    return r[reg];
}


void Regfile::execute() {
    enum gpreg port1 = reg_port1.read();
    enum gpreg port2 = reg_port2.read();
    enum gpreg port3 = reg_port3.read();
    bool port1_write = reg_port1_write.read();
    bool port2_write = reg_port2_write.read();

    if (port1_write) {
        write_reg(port1, reg_port1_data.read());
    }
    else {
        reg_port1_data.write(read_reg(port1));
    }

    if (port2_write) {
        write_reg(port2, reg_port2_data.read());
    }
    else {
        reg_port2_data.write(read_reg(port2));
    }

    write_reg(port3, reg_port3_data.read());
}
