from bus import MemoryBus
from ram import Ram
from interrupt import InterruptController
from constants import IRQ_BASE


class Machine:

    def __init__(self, clock) -> None:
        self.clock = clock
        self.bus = MemoryBus()
        self.ram = Ram(0x0000, 0x10000)
        self.bus.set_backing(self.ram)
        self.irq = InterruptController(IRQ_BASE)
        self._peripherals = []
        self.add(self.irq)


    def add(self, peripheral) -> None:
        peripheral.attach(self.clock, self.irq)
        self.bus.map(peripheral)
        self._peripherals.append(peripheral)


    def poll(self) -> None:
        for peripheral in self._peripherals:
            peripheral.poll()


    def reset(self) -> None:
        for peripheral in self._peripherals:
            peripheral.reset()
