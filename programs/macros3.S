        #macro add2(rda rdb rs1a rs1b rs2a rs2b)
        add rdb rs1b rs2b
        adc rda rs1a rs2a
        #endm

        add2(rx ry re rf ru rv)
