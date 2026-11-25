        .section vectors

        .addr Fault_Handler        // Reserved
        .addr Fault_Handler        // Reserved
        .addr Fault_Handler        // Reserved
        .addr Fault_Handler        // Reserved
        .addr Interrupt_Handler    // Interrupt handler
        .addr Syscall_Handler      // Syscall handler
        .addr Fault_Handler        // Fault handler
        .addr Reset_Handler        // Reset handler
