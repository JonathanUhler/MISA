#macro TCB_RA        0x00 #endm
#macro TCB_RB        0x01 #endm
#macro TCB_RC        0x02 #endm
#macro TCB_RD        0x03 #endm
#macro TCB_RE        0x04 #endm
#macro TCB_RF        0x05 #endm
#macro TCB_RU        0x06 #endm
#macro TCB_RV        0x07 #endm
#macro TCB_RW        0x08 #endm
#macro TCB_RX        0x09 #endm
#macro TCB_RY        0x0A #endm
#macro TCB_RZ        0x0B #endm
#macro TCB_RT        0x0C #endm
#macro TCB_SADDR     0x0D #endm
#macro TCB_RADDR     0x0F #endm
#macro TCB_FLAGS     0x11 #endm
#macro TCB_STATE     0x13 #endm
#macro TCB_PRIORITY  0x14 #endm
#macro TCB_WAKE_TICK 0x15 #endm
#macro TCB_LIST_NEXT 0x17 #endm
#macro TCB_SIZE      0x19 #endm


#macro TCB_STATE_READY   0 #endm
#macro TCB_STATE_RUNNING 1 #endm
#macro TCB_STATE_BLOCKED 2 #endm
