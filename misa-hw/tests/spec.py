import os
from pathlib import Path
import shutil
import subprocess
from subprocess import CompletedProcess
from tempfile import TemporaryDirectory

from memory import MEMORY_SIZE, MemMap
from state import MisaState, parse_spec_text


REPO_ROOT: Path = Path(__file__).resolve().parents[2]

EXE_ENV_VAR: str = "MISA_SPEC_EXE"
EXE_CANDIDATES: tuple = (
    REPO_ROOT / "install" / "misa-spec-exe",
    REPO_ROOT / "misa-spec" / ".lake" / "build" / "bin" / "misa-spec-exe"
)

RECORD_SEPARATOR: str = "---"

SWEEP_CHUNK: int = 1024


def find_spec_exe() -> Path:
    override: str = os.environ.get(EXE_ENV_VAR)
    if (override):
        path: Path = Path(override)
        if (not path.is_file()):
            raise FileNotFoundError(f"{EXE_ENV_VAR}={override} is not a file")
        return path

    found: str = shutil.which("misa-spec-exe")
    if (found):
        return Path(found)

    for candidate in EXE_CANDIDATES:
        if (candidate.is_file()):
            return candidate

    looked: str = ", ".join(str(candidate) for candidate in EXE_CANDIDATES)
    raise FileNotFoundError(
        f"cannot find misa-spec-exe; set {EXE_ENV_VAR}, put it on PATH, or build it with "
        f"`make install` (looked in {looked})"
    )


def format_steps(steps: int) -> str:
    return "-" if (steps is None) else str(steps)


def format_interrupts(interrupts: tuple) -> str:
    return "-" if (not interrupts) else ",".join(str(step) for step in interrupts)


def split_records(output: str) -> list:
    records: list = [[]]
    for line in output.split("\n"):
        if (line == RECORD_SEPARATOR):
            records.append([])
        else:
            records[-1].append(line)
    return ["\n".join(lines) for lines in records]


class SpecDriver:

    def __init__(self, exe: Path = None) -> None:
        self.exe: Path = exe or find_spec_exe()
        self._scratch: TemporaryDirectory = TemporaryDirectory(prefix = "misa-spec-")
        self._dir: Path = Path(self._scratch.name)
        self._image_path: Path = self._dir / "image.bin"
        self._map_path: Path = self._dir / "memmap.ld"
        self._state_path: Path = self._dir / "state.txt"
        self._jobs_path: Path = self._dir / "jobs.txt"
        self._image_cache: bytes = None
        self._map_cache: str = None


    def _write_image(self, image: bytes) -> None:
        if (len(image) != MEMORY_SIZE):
            raise ValueError(f"image is {len(image)} bytes, expected {MEMORY_SIZE}")
        if (image != self._image_cache):
            self._image_path.write_bytes(image)
            self._image_cache = image


    def _write_memmap(self, memmap: MemMap) -> None:
        text: str = memmap.to_spec_file()
        if (text != self._map_cache):
            self._map_path.write_text(text, encoding = "utf-8")
            self._map_cache = text


    def _run(self, args: list) -> str:
        result: CompletedProcess = subprocess.run(
            [str(self.exe)] + [str(arg) for arg in args], capture_output = True
        )
        if (result.returncode != 0):
            detail: str = result.stderr.decode("utf-8", errors = "replace").strip()
            raise RuntimeError(
                f"misa-spec-exe {' '.join(str(arg) for arg in args)} exited "
                f"{result.returncode}: {detail or 'no diagnostic'}"
            )
        return result.stdout.decode("utf-8")


    def q0(self, image: bytes, memmap: MemMap, extns: int, steps: int = None,
           interrupts: tuple = (), trace: bool = False) -> list:
        self._write_image(image)
        self._write_memmap(memmap)
        args: list = [
            "q0", self._image_path, format_steps(steps), str(extns),
            format_interrupts(interrupts), self._map_path
        ]
        if (trace):
            args.append("trace")
        return self._parse_states(self._run(args), cumulative = True)


    def state_file_text(self, image: bytes, memmap: MemMap, extns: int, steps: int = 0) -> str:
        self._write_image(image)
        self._write_memmap(memmap)
        return self._run([
            "q0", self._image_path, format_steps(steps), str(extns), "-", self._map_path
        ])


    def qi(self, state: MisaState, memmap: MemMap, steps: int = None,
           interrupts: tuple = (), trace: bool = False) -> list:
        self._state_path.write_text(state.to_spec_file(memmap), encoding = "utf-8")
        args: list = ["qi", self._state_path, format_steps(steps), format_interrupts(interrupts)]
        if (trace):
            args.append("trace")
        return self._parse_states(self._run(args), cumulative = True)


    def sweep(self, image: bytes, memmap: MemMap, extns: int, jobs: tuple,
              steps: int = None, interrupts: tuple = ()) -> tuple:
        self._write_image(image)
        self._write_memmap(memmap)
        self._jobs_path.write_text(format_jobs(jobs), encoding = "utf-8")
        output: str = self._run([
            "sweep", self._image_path, format_steps(steps), str(extns),
            format_interrupts(interrupts), self._map_path, self._jobs_path
        ])
        states: list = self._parse_states(output, cumulative = False)
        if (len(states) != len(jobs) + 1):
            raise RuntimeError(
                f"misa-spec sweep returned {len(states) - 1} records for {len(jobs)} jobs"
            )
        return (states[0], states[1:])


    def sweep_chunks(self, image: bytes, memmap: MemMap, extns: int, jobs: tuple,
                     steps: int = None, interrupts: tuple = (),
                     chunk: int = SWEEP_CHUNK) -> object:
        if (chunk < 1):
            raise ValueError(f"a sweep chunk of {chunk} jobs cannot make progress")

        for start in range(0, len(jobs), chunk):
            batch: tuple = jobs[start:start + chunk]
            _, finals = self.sweep(image, memmap, extns, batch, steps, interrupts)
            yield from zip(batch, finals)


    def _parse_states(self, output: str, cumulative: bool) -> list:
        records: list = split_records(output)
        base: MisaState = MisaState.from_record(parse_spec_text(records[0]))
        states: list = [base]
        for record in records[1:]:
            states.append(states[-1].apply_delta(record) if (cumulative)
                          else base.apply_delta(record))
        return states


def format_jobs(jobs: tuple) -> str:
    lines: list = []
    for job in jobs:
        lines.append("; ".join(
            f"{addr:#06x}: " + " ".join(f"{byte:#04x}" for byte in data) for addr, data in job
        ))
    return "\n".join(lines) + "\n"
