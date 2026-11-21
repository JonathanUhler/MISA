from amaranth import Array, Module, Signal, Value
from amaranth.lib.wiring import Component, In, Out

from .init import MpuInit


class MpuUnit(Component):

    write_data: In(16)
    write_enable: In(1)
    query_addr: In(16)

    read_data: Out(16)
    query_caps: Out(3)


    def __init__(self, init_data: MpuInit = None):
        init_data = init_data or MpuInit()
        self.regions: Array = Array([
            Signal(16, init = region) for region in init_data.regions
        ])
        self.index: Signal = Signal(2, init = init_data.index)
        super().__init__()


    def contains(self, region: Value) -> Value:
        size: Value = region[5:8]
        base: Value = region[8:16]
        return (self.query_addr[8:16] >> size) == (base >> size)


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        m.d.comb += self.read_data.eq(self.regions[self.index])

        m.d.comb += self.query_caps.eq(0b000)
        for index in reversed(range(len(self.regions))):
            with m.If(self.contains(self.regions[index])):
                m.d.comb += self.query_caps.eq(self.regions[index][0:3])

        with m.If(self.write_enable):
            m.d.sync += [
                self.regions[self.write_data[3:5]].eq(self.write_data),
                self.index.eq(self.write_data[3:5])
            ]

        return m
