        .section text
_start:
        set ra 0x00
loop:
        jmpi always loop            // spin until an interrupt fires

interrupt_handler:
        // read head interrupt number from IRQ_BASE+3 (0x8003) into RA and store to `last`
        set2 rc rd 0x8003
        ld ra rc rd
        set2 rc rd last
        st ra rc rd
        // four-phase pop: write pop=1 at 0x8001, wait ack (0x8002)=1, write pop=0, wait ack=0
        set2 rc rd 0x8001
        set rb 0x01
        st rb rc rd
wait_ack_high:
        set2 rc rd 0x8002
        ld rb rc rd
        cmp rb r0
        jmpi equal wait_ack_high
        set2 rc rd 0x8001
        st r0 rc rd
wait_ack_low:
        set2 rc rd 0x8002
        ld rb rc rd
        cmp rb r0
        jmpi not_equal wait_ack_low
        reti

        .section data
last:   .word 0x00

        .section vectors
        .space 8
        .addr interrupt_handler      // 0xFFF8
        .space 4
        .addr _start                 // 0xFFFE
