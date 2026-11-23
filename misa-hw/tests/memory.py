from abc import ABC, abstractmethod
from dataclasses import dataclass
from random import Random

from misa_hw.grammar import AccessKind


MEMORY_SIZE: int = 0x10000

CAPS_READ: int = AccessKind.READ
CAPS_WRITE: int = AccessKind.WRITE
CAPS_EXECUTE: int = AccessKind.EXECUTE
CAPS_RWX: int = CAPS_READ | CAPS_WRITE | CAPS_EXECUTE
CAPS_NAME_BITS: dict = (("r", CAPS_READ), ("w", CAPS_WRITE), ("x", CAPS_EXECUTE))


def caps_to_name(caps: int) -> str:
    name: str = "".join(letter for letter, bit in CAPS_NAME_BITS if (caps & bit))
    return name if (name) else "-"


def caps_from_name(name: str) -> int:
    return sum(bit for letter, bit in CAPS_NAME_BITS if (letter in name))


@dataclass(frozen = True)
class MemRegion:

    lo: int
    hi: int
    caps: int


    def __post_init__(self) -> None:
        if (not (0 <= self.lo < MEMORY_SIZE)) or (not (0 <= self.hi < MEMORY_SIZE)):
            raise ValueError(f"MemRegion({self.lo:#06x}, {self.hi:#06x}) is not a 16-bit range")
        if (self.hi < self.lo):
            raise ValueError(f"MemRegion({self.lo:#06x}, {self.hi:#06x}) ends before it starts")
        if (not (0 <= self.caps <= CAPS_RWX)):
            raise ValueError(f"MemRegion caps {self.caps:#05b} is not three bits")


    def contains(self, addr: int) -> bool:
        return (self.lo <= addr) and (addr <= self.hi)


class MemMap:

    def __init__(self, regions: tuple) -> None:
        if (not regions):
            raise ValueError("MemMap needs at least one region; use MemMap.everything()")
        self.regions: tuple = tuple(regions)


    @classmethod
    def everything(cls, caps: int = CAPS_RWX) -> "MemMap":
        return cls((MemRegion(0x0000, MEMORY_SIZE - 1, caps),))


    @classmethod
    def from_rows(cls, rows: tuple) -> "MemMap":
        return cls(tuple(MemRegion(lo, hi, caps_from_name(name)) for lo, hi, name in rows))


    def caps_at(self, addr: int) -> int:
        for region in self.regions:
            if (region.contains(addr)):
                return region.caps
        return None


    def to_spec_file(self) -> str:
        return "".join(
            f"memory {region.lo:#06x} - {region.hi:#06x} : {caps_to_name(region.caps)} ;\n"
            for region in self.regions
        )


    def to_state_file_section(self) -> str:
        edges: list = sorted({0} | {
            bound for region in self.regions
            for bound in (region.lo, region.hi + 1) if (bound < MEMORY_SIZE)
        })
        rows: list = []
        for index, lo in enumerate(edges):
            hi: int = (edges[index + 1] - 1) if (index + 1 < len(edges)) else (MEMORY_SIZE - 1)
            caps: int = self.caps_at(lo)
            if (caps is None):
                continue
            if (rows) and (rows[-1][2] == caps) and (rows[-1][1] + 1 == lo):
                rows[-1] = (rows[-1][0], hi, caps)
            else:
                rows.append((lo, hi, caps))
        return "".join(f"  {lo:#06x} - {hi:#06x} : {caps_to_name(caps)}\n" for lo, hi, caps in rows)


    @classmethod
    def from_state_file_rows(cls, rows: tuple) -> "MemMap":
        regions: list = []
        for key, value in rows:
            bounds: list = [part.strip() for part in key.split("-")]
            if (len(bounds) != 2):
                raise ValueError(f"caps row '{key}' is not an address range")
            regions.append(MemRegion(int(bounds[0], 0), int(bounds[1], 0), caps_from_name(value)))
        return cls(tuple(regions))


    def __eq__(self, other: object) -> bool:
        return isinstance(other, MemMap) and (self.regions == other.regions)


    def __repr__(self) -> str:
        rows: str = ", ".join(
            f"{region.lo:#06x}-{region.hi:#06x}:{caps_to_name(region.caps)}"
            for region in self.regions
        )
        return f"MemMap({rows})"


class MemoryLatency(ABC):

    name: str


    @abstractmethod
    def wait_cycles(self, addr: int, write: bool) -> int:
        ...


class ZeroLatency(MemoryLatency):

    name: str = "zero"


    def wait_cycles(self, addr: int, write: bool) -> int:
        return 0


class FixedLatency(MemoryLatency):

    def __init__(self, cycles: int) -> None:
        if (cycles < 0):
            raise ValueError(f"FixedLatency({cycles}) must not be negative")
        self.cycles: int = cycles
        self.name: str = f"fixed{cycles}"


    def wait_cycles(self, addr: int, write: bool) -> int:
        return self.cycles


class RandomLatency(MemoryLatency):

    def __init__(self, seed: int, maximum: int = 4) -> None:
        if (maximum < 0):
            raise ValueError(f"RandomLatency maximum {maximum} must not be negative")
        self.random: Random = Random(seed)
        self.maximum: int = maximum
        self.name: str = f"random{seed}"


    def wait_cycles(self, addr: int, write: bool) -> int:
        return self.random.randint(0, self.maximum)


@dataclass(frozen = True)
class Access:

    cycle: int
    addr: int
    write: bool
    data: int


class MemoryBfm:

    def __init__(self, image: bytes, memmap: MemMap, latency: MemoryLatency = None) -> None:
        if (len(image) != MEMORY_SIZE):
            raise ValueError(f"image is {len(image)} bytes, expected {MEMORY_SIZE}")
        self.memmap: MemMap = memmap
        self.latency: MemoryLatency = latency or ZeroLatency()
        self.memory: bytearray = bytearray(image)
        self.accesses: list = []
        self.writes: list = []
        self._image: bytes = bytes(image)
        self._snapshot: bytes = self._image
        self._snapshot_writes: int = 0
        self._pending: tuple = None
        self._waited: int = 0
        self._target: int = 0


    def reset(self, image: bytes) -> None:
        self.memory = bytearray(image)
        self.accesses = []
        self.writes = []
        self._image = bytes(image)
        self._snapshot = self._image
        self._snapshot_writes = 0
        self._pending = None
        self._waited = 0
        self._target = 0


    def snapshot(self) -> bytes:
        if (len(self.writes) != self._snapshot_writes):
            self._snapshot = bytes(self.memory)
            self._snapshot_writes = len(self.writes)
        return self._snapshot


    def responses(self, addr: int) -> tuple:
        caps: int = self.memmap.caps_at(addr)
        return (0, 0b000) if (caps is None) else (1, caps)


    def settle(self, cycle: int, addr: int, request: bool, write: bool, write_data: int) -> tuple:
        if (not request):
            self._pending = None
            self._waited = 0
            return (0, 0)

        key: tuple = (addr, write)
        if (key != self._pending):
            self._pending = key
            self._waited = 0
            self._target = self.latency.wait_cycles(addr, write)

        if (self._waited < self._target):
            self._waited += 1
            return (0, 0)

        data: int = write_data if (write) else self.memory[addr]
        self.accesses.append(Access(cycle, addr, write, data))
        if (write):
            self.memory[addr] = write_data
            self.writes.append((addr, write_data))
        self._pending = None
        self._waited = 0
        return (1, data)


    def addresses_touched(self) -> set:
        return {access.addr for access in self.accesses}
