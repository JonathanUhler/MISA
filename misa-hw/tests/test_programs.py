from dataclasses import dataclass
from pathlib import Path

from pytest import MarkDecorator, fixture, mark, raises, skip

from misa_hw.grammar import Extn

from compare import final, lockstep
from generators import DEFAULT_EXTNS
from memory import MemMap, MemoryLatency
from programs import (
    BRINGUP_CODES, BRINGUP_MEMMAP, FAULT_TAG, RESET_VECTOR, SYSCALL_VECTOR, bringup_image,
    bringup_names, expect_finish, rebuild_bringup, toolchain_env
)
from spec import SpecDriver
from state import MisaState
from target import RunResult, Target


pytestmark: MarkDecorator = mark.subset

HALT_LIMIT: int = 100000
PREFIX_STEPS: int = 10000
LONG_LIMIT: int = 2000000

BRINGUP_MAP_TEXT: str = BRINGUP_MEMMAP.read_text(encoding = "utf-8")

ONE_REGION_MAP: str = "memory 0x0000 - 0x00FF : rwx ;"
ONE_REGION_ROWS: tuple = ((0x0000, 0x00FF, "rwx"),)
MIXED_MAP: str = (
    "section 0x0000 - 0X00FF : direct ;   // skipped, and not even a valid address\n"
    "mpu 0 : 0x1234 ;                     /* skipped too */\n"
    "memory 0x0100 - 0x7fff : rw ;\n"
    "memory 0xC000 - 0xFFFF : - ;\n"
)
MIXED_ROWS: tuple = ((0x0100, 0x7FFF, "rw"), (0xC000, 0xFFFF, "-"))


@dataclass(frozen = True)
class Bringup:
    name: str
    purpose: str
    requires: int = 0x0
    halts: bool = True
    code: str = None


    @property
    def success(self) -> str:
        return f"{self.name.upper()}_SUCCESS"


    @property
    def expected(self) -> str:
        return self.code or self.success


    @property
    def steps(self) -> int:
        return HALT_LIMIT if (self.halts) else PREFIX_STEPS


    def extension_sets(self) -> tuple:
        return (self.requires,) if (self.requires == DEFAULT_EXTNS) \
            else (self.requires, DEFAULT_EXTNS)


PROGRAMS: tuple = (
    Bringup("firsttest", "the shortest program which can halt, and the first one to run"),
    Bringup("inttest",
            "the canonical interrupt handler. Nothing here drives the interrupt pin, so the loop "
            "runs its rounds untouched and reports that no interrupt arrived; the interrupt "
            "timing campaign drives the pin and expects INTTEST_SUCCESS from the same image",
            requires = Extn.INTERRUPT, code = "INTTEST_NO_INTERRUPT"),
    Bringup("memtest", "writes and reads back every byte below the program", halts = False),
    Bringup("mputest", "four stages of memory protection unit reconfiguration in User Mode",
            requires = Extn.PRIVILEGE),
    Bringup("synctest", "a system call round trip", requires = Extn.SYSCALL)
)

RUNS: tuple = tuple(
    (program, extns) for program in PROGRAMS for extns in program.extension_sets()
)
SHORT_RUNS: tuple = tuple(run for run in RUNS if (run[0].halts))
LONG_RUNS: tuple = tuple(run for run in RUNS if (not run[0].halts))
LONG_PROGRAMS: tuple = tuple(program for program in PROGRAMS if (not program.halts))


def label(value: object) -> str:
    return value.name if (isinstance(value, Bringup)) else f"extns{value:#x}"


def test_the_campaign_covers_every_bringup_program() -> None:
    named: tuple = tuple(program.name for program in PROGRAMS)
    assert named == bringup_names(), (
        f"the campaign runs {list(named)} and programs/bringup holds "
        f"{list(bringup_names())}; a program nobody runs is a program nobody trusts"
    )


def test_every_program_names_a_success_code(codes: dict) -> None:
    assert codes[FAULT_TAG] == 0x80, "a tagged finish code is one with the top bit set"

    reported: dict = {}
    for program in PROGRAMS:
        assert program.success in codes, (
            f"{BRINGUP_CODES.name} has no {program.success}, which is the code "
            f"{program.name} reports when every check it makes has passed"
        )
        assert program.expected in codes, (
            f"{BRINGUP_CODES.name} has no {program.expected}, which is the code this campaign "
            f"expects {program.name} to finish with"
        )
        code: int = codes[program.success]
        assert code < codes[FAULT_TAG], (
            f"{program.success} is {code:#04x}, which a fault reported by the fault handler could "
            f"also produce"
        )
        assert code not in reported, (
            f"{program.success} and {reported.get(code)} are both {code:#04x}, so one program "
            f"passing cannot be told from another"
        )
        reported[code] = program.success

    for name, code in codes.items():
        assert 0x00 <= code <= 0xFF, f"the finish code {name} is {code:#x}, which is not a byte"


def test_a_program_memory_map_with_no_memory_statement_maps_everything(
        bringup_memmap: MemMap) -> None:
    assert bringup_memmap == MemMap.everything(), (
        "the bringup memory map leaves its memory statement commented out, which describes a "
        "processor that maps its whole address space and grants everything there"
    )


@mark.parametrize("text, rows", [
    (ONE_REGION_MAP, ONE_REGION_ROWS), (MIXED_MAP, MIXED_ROWS)
], ids = ["one-region", "mixed-statements"])
def test_a_program_memory_map_parses_the_way_misa_ld_wrote_it(text: str, rows: tuple) -> None:
    assert MemMap.from_memmap_text(text) == MemMap.from_rows(rows)


@mark.parametrize("text, problem", [
    ("region 0x0000 - 0x00FF : rwx ;", "unknown memory map statement 'region'"),
    ("memory 0x0000 - 0x00FF : rwx", "with no semicolon"),
    ("memory 0x0000 : rwx ;", "is not an address range"),
    ("0x0000 - 0x00FF : rwx ;", "is not a memory map statement")
], ids = ["unknown", "unterminated", "malformed", "nameless"])
def test_a_malformed_program_memory_map_is_refused(text: str, problem: str) -> None:
    with raises(ValueError, match = problem):
        MemMap.from_memmap_text(text)


@mark.parametrize("text", [BRINGUP_MAP_TEXT, ONE_REGION_MAP, MIXED_MAP],
                  ids = ["bringup", "one-region", "mixed-statements"])
def test_a_program_memory_map_is_the_one_misa_spec_reads(spec: SpecDriver, tmp_path: Path,
                                                         text: str) -> None:
    path: Path = tmp_path / BRINGUP_MEMMAP.name
    path.write_text(text, encoding = "utf-8")

    mine: MemMap = MemMap.from_memmap_text(text)
    oracle: MemMap = spec.memmap_of_file(bringup_image(PROGRAMS[0].name), path)
    assert mine.to_state_file_section() == oracle.to_state_file_section()


@mark.parametrize("program, extns", RUNS, ids = label)
def test_a_bringup_program_runs_in_lockstep(target: Target, spec: SpecDriver,
                                            bringup_memmap: MemMap, latency: MemoryLatency,
                                            program: Bringup, extns: int) -> None:
    image: bytes = bringup_image(program.name)
    states: list = lockstep(target, spec, image, bringup_memmap, steps = program.steps,
                            extns = extns, latency = latency)

    reset: int = image[RESET_VECTOR] | (image[RESET_VECTOR + 1] << 8)
    assert states[0].pc == reset, (
        f"{program.name} began at pc={states[0].pc:#06x} and its reset vector names {reset:#06x}"
    )

    if (program.halts):
        assert states[-1].halted, (
            f"{program.name} ({program.purpose}) ran {len(states) - 1} instructions without "
            f"halting, and ended at pc={states[-1].pc:#06x}"
        )
    else:
        assert len(states) == program.steps + 1, (
            f"{program.name} halted after {len(states) - 1} instructions, where the campaign "
            f"follows it for a prefix because it is too long to run to the end"
        )


@mark.parametrize("program, extns", SHORT_RUNS, ids = label)
def test_a_bringup_program_halts_with_its_finish_code(target: Target, bringup_memmap: MemMap,
                                                      latency: MemoryLatency, codes: dict,
                                                      program: Bringup, extns: int) -> None:
    result: RunResult = target.run(bringup_image(program.name), None, program.steps, (),
                                   bringup_memmap, latency, reset_extns = extns)
    expect_finish(program.name, codes[program.expected], result.states, codes)


@mark.parametrize("program, extns", LONG_RUNS, ids = label)
def test_a_long_program_reaches_its_finish_code_in_the_oracle(spec: SpecDriver,
                                                              bringup_memmap: MemMap,
                                                              codes: dict, program: Bringup,
                                                              extns: int) -> None:
    states: list = spec.q0(bringup_image(program.name), bringup_memmap, extns, steps = LONG_LIMIT)
    expect_finish(program.name, codes[program.expected], states, codes)


@mark.exhaustive
@mark.slow
@mark.parametrize("program", LONG_PROGRAMS, ids = label)
def test_a_long_program_reaches_its_finish_code_on_the_design(target: Target, spec: SpecDriver,
                                                              bringup_memmap: MemMap,
                                                              latency: MemoryLatency, codes: dict,
                                                              program: Bringup) -> None:
    ended: MisaState = final(target, spec, bringup_image(program.name), bringup_memmap,
                             steps = LONG_LIMIT, extns = DEFAULT_EXTNS, latency = latency)
    expect_finish(program.name, codes[program.expected], [ended], codes)


def test_the_finish_check_notices_a_program_which_failed(target: Target, bringup_memmap: MemMap,
                                                         codes: dict) -> None:
    program: Bringup = next(entry for entry in PROGRAMS if (entry.name == "synctest"))
    broken: bytearray = bytearray(bringup_image(program.name))
    broken[SYSCALL_VECTOR:SYSCALL_VECTOR + 2] = b"\x00\x00"

    result: RunResult = target.run(bytes(broken), None, program.steps, (), bringup_memmap, None,
                                   reset_extns = DEFAULT_EXTNS)
    with raises(AssertionError, match = "SYNCTEST_SUCCESS"):
        expect_finish(program.name, codes[program.expected], result.states, codes)


def test_the_checked_in_binaries_match_their_sources(tmp_path: Path) -> None:
    if (toolchain_env() is None):
        skip("make and misa-as are both needed to assemble the bringup programs")

    built: dict = rebuild_bringup(tmp_path)
    assert tuple(sorted(built)) == bringup_names(), (
        f"the Makefile built {sorted(built)} out of the sources of {list(bringup_names())}"
    )

    for name, image in built.items():
        assert image == bringup_image(name), (
            f"programs/bringup/bin/{name}.bin is not what {name}.S assembles to, so the campaign "
            f"is running a program nobody can rebuild"
        )
