from constants import WORD_MASK


class Ram:

    def __init__(self, base: int = 0x0000, size: int = 0x10000) -> None:
        self.base = base
        self.size = size
        self.data = [0] * size


    def read(self, offset: int) -> int:
        return self.data[offset]


    def write(self, offset: int, value: int) -> None:
        self.data[offset] = value & WORD_MASK
