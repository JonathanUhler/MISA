    .section text
_start:                   // _start is a symbol
    SET2 RSCRATCH 0x01FF
    WSR SADDR RSCRATCH
    CALL main             // main is a relocation
    HALT RT
