#ifndef _PROCESSOR_H_
#define _PROCESSOR_H_


#include <stdbool.h>
#include <stdint.h>


enum stage {
    FETCHLO = 0b00,
    FETCHHI = 0b01,
    EXECUTE = 0b10
};


struct proc {
    struct regfile *regfile;
    struct csrfile *csrfile;
    struct memory *memory;
    enum stage stage;
    uint16_t pc;
    uint16_t inst;
    bool reset;
};


enum proc_status {
    PROC_SUCCESS,
    PROC_INVALID_ARGUMENT,
    PROC_IN_RESET
};


enum proc_status proc_raise_reset(struct proc *proc);


enum proc_status proc_lower_reset(struct proc *proc);


enum proc_status proc_step(struct proc *proc);


#endif  // _PROCESSOR_H_
