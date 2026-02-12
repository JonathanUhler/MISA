        .section text
_start:
        LDI SPHI SPLO 0xABCD
        LDI RAHI RALO main      
        SA RAHI RALO
        JLZ ZERO 0
        HALT 0
