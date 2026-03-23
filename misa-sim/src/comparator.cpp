#include "comparator.h"

#include <cstdint>


void Comparator::execute() {
    uint16_t flags = cmp_flags.read();
    bool z = flags & 0b0001;
    bool n = flags & 0b0100;
    bool v = flags & 0b1000;

    bool equal = z;
    bool less = n ^ v;

    cmp_always.write(true);
    cmp_equal.write(equal);
    cmp_notequal.write(!equal);
    cmp_greater.write(!less && !equal);
    cmp_less.write(less);
    cmp_greaterequal.write(!less);
    cmp_lessequal.write(less || equal);
}
