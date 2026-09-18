from interrupt import InterruptController, InterruptStruct
from constants import IRQ_BASE


def test_raise_enqueues_and_asserts_line():
    c = InterruptController(IRQ_BASE)
    assert c.pending() is False
    c.raise_irq(0x07, 0xBEEF)
    assert c.pending() is True
    assert c.read(InterruptStruct.LENGTH) == 1
    assert c.read(InterruptStruct.NUMBER) == 0x07
    assert c.read(InterruptStruct.ARGPTR_LO) == 0xEF
    assert c.read(InterruptStruct.ARGPTR_HI) == 0xBE


def test_acknowledge_clears_line():
    c = InterruptController(IRQ_BASE)
    c.raise_irq(0x01)
    c.acknowledge()
    assert c.pending() is False


def test_four_phase_pop_advances_head():
    c = InterruptController(IRQ_BASE)
    c.raise_irq(0x07, 0xBEEF)
    c.raise_irq(0x09, 0x1234)
    assert c.read(InterruptStruct.NUMBER) == 0x07
    c.write(InterruptStruct.POP, 0x01)          # rising edge, pop head, ack = 1
    assert c.read(InterruptStruct.ACK) == 1
    assert c.read(InterruptStruct.LENGTH) == 1
    assert c.read(InterruptStruct.NUMBER) == 0x09
    c.write(InterruptStruct.POP, 0x00)          # falling edge, ack = 0
    assert c.read(InterruptStruct.ACK) == 0
