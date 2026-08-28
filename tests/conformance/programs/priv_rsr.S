        .section text
_start:
        set ra 0x00
        set rb 0x01
        wsr privs ra rb        // enter User Mode, ring bit set
        rsr privs rc rd        // read PRIVS in User Mode, the operation under test
        halt rd                // HALT in User Mode faults into the handler below

__fault:
        halt r0

        .section vectors
        .space 12
        .addr __fault
        .addr _start
