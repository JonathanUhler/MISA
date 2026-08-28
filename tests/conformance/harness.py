"""
Conformance test harness for the MISA reference simulator.

This module drives the real MISA toolchain. It assembles and links small assembly programs into flat
binaries and runs them in the reference simulator. Tests import the helpers here to build a program
from source and inspect the final processor state.

The built toolchain must exist in the `install` directory at the repository root. Run `make` from
the root before running the conformance suite. See the README in this directory for details.

Author: MISA conformance suite
"""


import os
from pathlib import Path
import subprocess
from subprocess import CompletedProcess
import sys
import tempfile
from typing import Final


REPO_ROOT: Final = Path(__file__).resolve().parents[2]
INSTALL_DIR: Final = REPO_ROOT / "install"
SIM_DIR: Final = REPO_ROOT / "misa-sim"
PROGRAMS_DIR: Final = Path(__file__).resolve().parent / "programs"


# The simulator is imported from the source tree so tests check the live implementation rather than
# the copy placed in the install directory during a build.
sys.path.insert(0, str(SIM_DIR))
from simulator import (  # noqa: E402
    Simulator, Reg, Csr, Cmp, CauseReason, CauseTypeInstruction, CauseTypeMemory
)


def _toolchain_env() -> dict:
    """
    Builds an environment with the install directory on the PATH.

    Returns:
      dict: A copy of the current environment with the MISA install directory prepended to PATH, so
            the misa-as wrapper and the Haskell -exe binaries can be found.
    """

    env: dict = os.environ.copy()
    env["PATH"] = f"{INSTALL_DIR}{os.pathsep}{env.get('PATH', '')}"
    return env


def assemble(source: str, workdir: str, extensions: list = None) -> Path:
    """
    Assembles and links MISA source into a flat binary.

    The source string is written to a file in workdir and passed to the misa-as wrapper, which
    preprocesses, assembles, and links it with the default memory map. The linked flat binary is
    returned.

    Arguments:
      source (str):      The MISA assembly source to build.
      workdir (str):     A writable directory for intermediate and output files.
      extensions (list): Architecture extensions to enable in the assembler, such as ["privilege"].
                         These map to the -e flag of misa-as.

    Returns:
      Path: The path to the linked flat binary.
    """

    src_path: Path = Path(workdir) / "program.S"
    src_path.write_text(source)
    bin_path: Path = Path(workdir) / "program.bin"

    command: list = ["misa-as", str(src_path), "-o", str(bin_path)]
    if (extensions):
        command += ["-e", *extensions]

    result: CompletedProcess = subprocess.run(
        command,
        cwd = workdir,
        env = _toolchain_env(),
        capture_output = True,
        text = True
    )
    if (result.returncode != 0):
        raise AssertionError(f"assembly failed\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}")
    return bin_path


def load_source(source: str, extensions: list = None) -> Simulator:
    """
    Assembles MISA source and loads it into a fresh simulator at its reset state.

    The program is built with assemble() and loaded into a new simulator. The simulator is left at
    the reset vector without stepping, so a test may seed memory or single-step by hand before
    inspecting state.

    Arguments:
      source (str):      The MISA assembly source to build.
      extensions (list): Architecture extensions to enable in the assembler.

    Returns:
      Simulator: A fresh simulator with the program loaded, positioned at the reset vector.
    """

    with tempfile.TemporaryDirectory() as workdir:
        bin_path: Path = assemble(source, workdir, extensions)
        sim: Simulator = Simulator()
        sim.load_mem(str(bin_path))
        return sim


def step_to_halt(sim: Simulator, max_steps: int = 1000) -> Simulator:
    """
    Steps a loaded simulator until it halts.

    Arguments:
      sim (Simulator): A simulator positioned at the reset vector.
      max_steps (int): The maximum number of instructions to simulate before giving up.

    Returns:
      Simulator: The halted simulator, ready for state inspection.
    """

    steps: int = 0
    while (not sim.in_reset and steps < max_steps):
        sim.step()
        steps += 1
    if (steps >= max_steps):
        raise AssertionError(f"program did not halt within {max_steps} steps")
    return sim


def run_source(source: str, max_steps: int = 1000, extensions: list = None) -> Simulator:
    """
    Assembles, loads, and runs MISA source in the reference simulator.

    The program is built with assemble(), loaded into a fresh simulator, and stepped until it halts
    or until max_steps instructions have run. A program that does not halt in time fails the test.

    Arguments:
      source (str):      The MISA assembly source to run.
      max_steps (int):   The maximum number of instructions to simulate before giving up.
      extensions (list): Architecture extensions to enable in the assembler.

    Returns:
      Simulator: The halted simulator, ready for state inspection.
    """

    return step_to_halt(load_source(source, extensions), max_steps)


TEXT_HEADER: Final = "        .section text\n_start:\n"
VECTORS_FOOTER: Final = "        .section vectors\n        .space 14\n        .addr _start\n"


def build_program(body: str, data: str = None) -> str:
    """
    Wraps a body of instructions into a complete, linkable program.

    The body becomes the text section and runs from a `_start` label that the reset vector points
    to. An optional data section is appended when `data` is given, which seeds known bytes in memory
    for load tests. This keeps a single-instruction test to a short string rather than its own
    fixture file.

    Arguments:
      body (str): The instructions and labels of the text section.
      data (str): Optional contents of a data section, such as a labeled `.word` directive.

    Returns:
      str: A full assembly program with a reset vector targeting `_start`.
    """

    program: str = TEXT_HEADER + body.strip("\n") + "\n\n"
    if (data is not None):
        program += "        .section data\n" + data.strip("\n") + "\n\n"
    program += VECTORS_FOOTER
    return program


def run(body: str, max_steps: int = 1000, extensions: list = None, data: str = None) -> Simulator:
    """
    Builds a program from a body of instructions and runs it to a halt.

    This is the common entry point for single-instruction tests. The body is wrapped with
    build_program(), so a test writes only the instructions under test and a terminating `HALT`.

    Arguments:
      body (str):        The instructions and labels of the text section.
      max_steps (int):   The maximum number of instructions to simulate before giving up.
      extensions (list): Architecture extensions to enable in the assembler.
      data (str):        Optional contents of a data section.

    Returns:
      Simulator: The halted simulator, ready for state inspection.
    """

    return run_source(build_program(body, data), max_steps, extensions)


def load(body: str, extensions: list = None, data: str = None) -> Simulator:
    """
    Builds a program from a body of instructions and loads it without running.

    This mirrors run() but returns the simulator at its reset state, so a test may seed memory or
    single-step by hand before inspecting state.

    Arguments:
      body (str):        The instructions and labels of the text section.
      extensions (list): Architecture extensions to enable in the assembler.
      data (str):        Optional contents of a data section.

    Returns:
      Simulator: A fresh simulator with the program loaded, positioned at the reset vector.
    """

    return load_source(build_program(body, data), extensions)


def run_program(name: str, max_steps: int = 1000, extensions: list = None) -> Simulator:
    """
    Runs a named program fixture from the programs directory.

    Arguments:
      name (str):        The file name of the fixture in the programs directory, such as
                         "arithmetic.S".
      max_steps (int):   The maximum number of instructions to simulate before giving up.
      extensions (list): Architecture extensions to enable in the assembler.

    Returns:
      Simulator: The halted simulator, ready for state inspection.
    """

    source: str = (PROGRAMS_DIR / name).read_text()
    return run_source(source, max_steps, extensions)
