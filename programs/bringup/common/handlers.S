        .section text

Fault_Handler:
        // Loop in the fault handler so ui_in[6:4] may be set to inspect core state.
        GOTO Fault_Handler

        // Unreachable, but should halt if the GOTO/branching is broken for whatever reason.
        SET RA 0xFF
        HALT RA

        // Even more unreachable, but the most canonical HALT encoding repeated as an instruction
        // and memory literal.
        HALT R0
        .word 0x00
        .word 0x00
