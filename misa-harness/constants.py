INST_SIZE: int = 16
CSR_SIZE: int  = 16
ADDR_SIZE: int = 16
WORD_SIZE: int =  8
NIB_SIZE: int  =  4

CSR_MASK: int  = 2 ** CSR_SIZE - 1
ADDR_MASK: int = 2 ** ADDR_SIZE - 1
WORD_MASK: int = 2 ** WORD_SIZE - 1
SIGN_MASK: int = 2 ** (WORD_SIZE - 1)

IRQ_BASE: int   = 0x8000
UART_BASE: int  = 0x8010
TIMER_BASE: int = 0x8020
