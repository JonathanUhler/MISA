class MemoryBus:

    def __init__(self) -> None:
        self._backing = None
        self._regions = []      # peripheral overlays, each has base, size, read, write


    def set_backing(self, target: object) -> None:
        self._backing = target


    def map(self, target: object) -> None:
        self._regions.append(target)


    def _decode(self, addr: int) -> (object, int):
        for region in self._regions:
            if (region.base <= addr < region.base + region.size):
                return region, addr - region.base
        return self._backing, addr


    def read(self, addr: int) -> int:
        target, offset = self._decode(addr)
        return target.read(offset)


    def write(self, addr: int, value: int) -> None:
        target, offset = self._decode(addr)
        target.write(offset, value)
