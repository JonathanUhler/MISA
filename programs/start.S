        .section text
_start:
        SETD RYZ 0xABCD
        WSR SADDR RYZ

        SETD RYZ main
        JAL ALWAYS RYZ

        HALT RT
