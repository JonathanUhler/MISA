#include "csrfile.h"
#include "grammar.h"
#include "memory.h"
#include "processor.h"
#include "regfile.h"

#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>


enum proc_status proc_raise_reset(struct proc *proc) {
    if (proc == NULL) {
        return PROC_INVALID_ARGUMENT;
    }
    proc->reset = true;
    return PROC_SUCCESS;
}


enum proc_status proc_lower_reset(struct proc *proc) {
    if (proc == NULL) {
        return PROC_INVALID_ARGUMENT;
    }
    proc->stage = FETCHHI;
    proc->pc = 0xFFFE;
    proc->reset = false;
    return PROC_SUCCESS;
}


static inline enum op proc_decode_op(struct proc *proc) {
    return (enum op) (proc->inst & 0xF);
}


static inline uint8_t proc_decode_imm(struct proc *proc) {
    return (proc->inst >> CHAR_BIT) & 0xFF;
}


static inline enum gpreg proc_decode_gpreg(struct proc *proc, uint8_t nibble_index) {
    return (enum gpreg) ((proc->inst >> (4 * nibble_index)) & 0xF);
}


static inline enum csrreg proc_decode_csrreg(struct proc *proc, uint8_t nibble_index) {
    return (enum csrreg) ((proc->inst >> (4 * nibble_index)) & 0xF);
}


static inline enum cmpflag proc_decode_cmpflag(struct proc *proc, uint8_t nibble_index) {
    return (enum cmpflag) ((proc->inst >> (4 * nibble_index)) & 0xF);
}


static void proc_decode(struct proc *proc, union inst *inst) {
    if (proc == NULL || inst == NULL) {
        return;
    }

    enum op op = proc_decode_op(proc);
    switch (op) {
    case HALT:
        inst->halt.op = op;
        inst->halt.rd = proc_decode_gpreg(proc, 1);
        break;
    case ADD:
        inst->add.op = op;
        inst->add.rd = proc_decode_gpreg(proc, 1);
        inst->add.rs1 = proc_decode_gpreg(proc, 2);
        inst->add.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case ADC:
        inst->adc.op = op;
        inst->adc.rd = proc_decode_gpreg(proc, 1);
        inst->adc.rs1 = proc_decode_gpreg(proc, 2);
        inst->adc.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case SUB:
        inst->sub.op = op;
        inst->sub.rd = proc_decode_gpreg(proc, 1);
        inst->sub.rs1 = proc_decode_gpreg(proc, 2);
        inst->sub.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case SBB:
        inst->sbb.op = op;
        inst->sbb.rd = proc_decode_gpreg(proc, 1);
        inst->sbb.rs1 = proc_decode_gpreg(proc, 2);
        inst->sbb.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case AND:
        inst->and.op = op;
        inst->and.rd = proc_decode_gpreg(proc, 1);
        inst->and.rs1 = proc_decode_gpreg(proc, 2);
        inst->and.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case OR:
        inst->or.op = op;
        inst->or.rd = proc_decode_gpreg(proc, 1);
        inst->or.rs1 = proc_decode_gpreg(proc, 2);
        inst->or.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case XOR:
        inst->xor.op = op;
        inst->xor.rd = proc_decode_gpreg(proc, 1);
        inst->xor.rs1 = proc_decode_gpreg(proc, 2);
        inst->xor.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case RRC:
        inst->rrc.op = op;
        inst->rrc.rd = proc_decode_gpreg(proc, 1);
        inst->rrc.rs1 = proc_decode_gpreg(proc, 2);
        break;
    case LW:
        inst->lw.op = op;
        inst->lw.rd = proc_decode_gpreg(proc, 1);
        inst->lw.rs1 = proc_decode_gpreg(proc, 2);
        inst->lw.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case SW:
        inst->sw.op = op;
        inst->sw.rd = proc_decode_gpreg(proc, 1);
        inst->sw.rs1 = proc_decode_gpreg(proc, 2);
        inst->sw.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case RSR:
        inst->rsr.op = op;
        inst->rsr.csr = proc_decode_csrreg(proc, 1);
        inst->rsr.rs1 = proc_decode_gpreg(proc, 2);
        inst->rsr.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case WSR:
        inst->wsr.op = op;
        inst->wsr.csr = proc_decode_csrreg(proc, 1);
        inst->wsr.rs1 = proc_decode_gpreg(proc, 2);
        inst->wsr.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case SET:
        inst->set.op = op;
        inst->set.rd = proc_decode_gpreg(proc, 1);
        inst->set.imm = proc_decode_imm(proc);
        break;
    case JAL:
        inst->jal.op = op;
        inst->jal.cmp = proc_decode_cmpflag(proc, 1);
        inst->jal.rs1 = proc_decode_gpreg(proc, 2);
        inst->jal.rs2 = proc_decode_gpreg(proc, 3);
        break;
    case JMP:
        inst->jmp.op = op;
        inst->jmp.cmp = proc_decode_cmpflag(proc, 1);
        inst->jmp.rs1 = proc_decode_gpreg(proc, 2);
        inst->jmp.rs2 = proc_decode_gpreg(proc, 3);
        break;
    }
}


static void proc_execute(struct proc *proc, union inst *inst) {
    if (proc == NULL || inst == NULL) {
        return;
    }

    switch (inst->CHECK_OP.op) {
    case HALT:
        csrfile_write(proc->csrfile, CAUSE, );
        break;
    case ADD:
        break;
    case ADC:
        break;
    case SUB:
        break;
    case SBB:
        break;
    case AND:
        break;
    case OR:
        break;
    case XOR:
        break;
    case RRC:
        break;
    case SET:
        break;
    case LW:
        break;
    case SW:
        break;
    case RSR:
        break;
    case WSR:
        break;
    case JAL:
        break;
    case JMP
        break;
    }
}


enum proc_status proc_step(struct proc *proc) {
    if (proc == NULL) {
        return PROC_INVALID_ARGUMENT;
    }
    if (proc->reset) {
        return PROC_IN_RESET;
    }

    switch (proc->stage) {
    case FETCHLO:
        uint8_t instlo = memory_read(proc->memory, proc->pc);
        proc->inst = instlo;
        (proc->pc)++;
        break;
    case FETCHHI:
        uint8_t insthi = memory_read(proc->memory, proc->pc);
        proc->inst |= insthi << CHAR_BIT;
        (proc->pc)++;
        break;
    case EXECUTE:
        union inst inst = {0};
        proc_decode(proc, &inst);
        proc_execute(proc, &inst);
        break;
    }

    proc->stage = (proc->stage + 1) % (EXECUTE - FETCHLO + 1);

    return PROC_SUCCESS;
}
