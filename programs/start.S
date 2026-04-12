        .section text
_start:
        SET2 RYZ 0xABCD
        WSR SADDR RYZ

        SET2 RYZ main
        JAL ALWAYS RYZ

        HALT RT
