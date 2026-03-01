#ifndef _GRAMMAR_H_
#define _GRAMMAR_H_


#include <stdint.h>


enum op {
    ADD,
    ADC,
    SUB,
    SBB,
    AND,
    OR,
    XOR,
    RRC,
    LW,
    SW,
    RSR,
    WSR,
    SET,
    JAL,
    JMP,
    HALT
};


enum gpreg {
    R0,
    RA,
    RB,
    RC,
    RD,
    RE,
    RF,
    RG,
    RH,
    RU,
    RV,
    RW,
    RX,
    RY,
    RZ,
    RT
};


enum csrreg {
    SADDR,
    RADDR,
    FLAGS
};


enum cmpflag {
    ALWAYS,
    EQUAL,
    NOT_EQUAL,
    GREATER,
    LESS,
    GREATER_EQUAL,
    LESS_EQUAL
};


union inst {
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } add;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } adc;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } sub;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } sbb;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } and;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } or;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } xor;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
    } rrc;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } lw;
    struct {
        enum op op;
        enum gpreg rd;
        enum gpreg rs1;
        enum gpreg rs2;
    } sw;
    struct {
        enum op op;
        enum csrreg csr;
        enum gpreg rs1;
        enum gpreg rs2;
    } rsr;
    struct {
        enum op op;
        enum csrreg csr;
        enum gpreg rs1;
        enum gpreg rs2;
    } wsr;
    struct {
        enum op op;
        enum gpreg rd;
        uint8_t imm;
    } set;
    struct {
        enum op op;
        enum cmpflag cmp;
        enum gpreg rs1;
        enum gpreg rs2;
    } jal;
    struct {
        enum op op;
        enum cmpflag cmp;
        enum gpreg rs1;
        enum gpreg rs2;
    } jmp;
    struct {
        enum op op;
        enum gpreg rd;
    } halt;
};


#endif  // _GRAMMAR_H_
