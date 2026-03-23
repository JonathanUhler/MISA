#include "arithmetic.h"

#include <cstdint>


void Arithmetic::execute() {
    bool cin = alu_cin.read();
    uint8_t a = alu_a.read();
    uint8_t b = alu_b.read();

    bool z = false;
    bool cout = false;
    bool n = false;
    bool v = false;
    uint8_t y = 0;

    if (alu_negb.read()) {
        b = ~b;
    }

    if (alu_and.read()) {
        y |= a & b;
    }
    if (alu_or.read()) {
        y |= a | b;
    }
    if (alu_xor.read()) {
        y |= a ^ b;
    }
    if (alu_rot.read()) {
        y |= (a << 1) | cin;
        cout |= a >> (CHAR_BIT - 1);
    }
    if (alu_add.read()) {
        uint16_t wide_y = (uint16_t) a + (uint16_t) b + (uint16_t) cin;
        y |= (uint8_t) wide_y;
        cout |= wide_y >> (CHAR_BIT - 1);
    }

    bool a_sign = a >> (CHAR_BIT - 1);
    bool b_sign = b >> (CHAR_BIT - 1);
    bool y_sign = y >> (CHAR_BIT - 1);

    z |= y == 0;
    n |= y_sign;
    v |= (a_sign == b_sign) && (a_sign != y_sign);

    alu_z.write(z);
    alu_cout.write(cout);
    alu_n.write(n);
    alu_v.write(v);
    alu_y.write(y);
}
