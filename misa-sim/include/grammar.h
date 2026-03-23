#ifndef _GRAMMAR_H_
#define _GRAMMAR_H_


#include <stdint.h>


enum op {
    HALT = 0x0,
    ADD  = 0x1,
    ADC  = 0x2,
    SUB  = 0x3,
    SBB  = 0x4,
    AND  = 0x5,
    OR   = 0x6,
    XOR  = 0x7,
    RRC  = 0x8,
    SET  = 0x9,
    LW   = 0xA,
    SW   = 0xB,
    RSR  = 0xC,
    WSR  = 0xD,
    JAL  = 0xE,
    JMP  = 0xF,

    OP_MAX
};


enum gpreg {
    R0 = 0x0,
    RA = 0x1,
    RB = 0x2,
    RC = 0x3,
    RD = 0x4,
    RE = 0x5,
    RF = 0x6,
    RG = 0x7,
    RH = 0x8,
    RU = 0x9,
    RV = 0xA,
    RW = 0xB,
    RX = 0xC,
    RY = 0xD,
    RZ = 0xE,
    RT = 0xF,

    GPREG_MAX
};


enum csrreg {
    SADDR = 0x1,
    RADDR = 0x2,
    FLAGS = 0x3,
    CAUSE = 0x4,

    CSRREG_MAX
};


enum cmpflag {
    ALWAYS        = 0x0,
    EQUAL         = 0x1,
    NOT_EQUAL     = 0x8,
    GREATER       = 0x2,
    LESS          = 0x4,
    GREATER_EQUAL = 0x3,
    LESS_EQUAL    = 0x5,

    CMPFLAG_MAX
};


#endif  // _GRAMMAR_H_
