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
from simulator import Simulator, Reg, Csr, Cmp  # noqa: E402


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


def assemble(source: str, workdir: str) -> Path:
    """
    Assembles and links MISA source into a flat binary.

    The source string is written to a file in workdir and passed to the misa-as wrapper, which
    preprocesses, assembles, and links it with the default memory map. The linked flat binary is
    returned.

    Arguments:
      source (str):  The MISA assembly source to build.
      workdir (str): A writable directory for intermediate and output files.

    Returns:
      Path: The path to the linked flat binary.
    """

    src_path: Path = Path(workdir) / "program.S"
    src_path.write_text(source)
    bin_path: Path = Path(workdir) / "program.bin"

    result: CompletedProcess = subprocess.run(
        ["misa-as", str(src_path), "-o", str(bin_path)],
        cwd = workdir,
        env = _toolchain_env(),
        capture_output = True,
        text = True
    )
    if (result.returncode != 0):
        raise AssertionError(f"assembly failed\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}")
    return bin_path


def run_source(source: str, max_steps: int = 1000) -> Simulator:
    """
    Assembles, loads, and runs MISA source in the reference simulator.

    The program is built with assemble(), loaded into a fresh simulator, and stepped until it halts
    or until max_steps instructions have run. A program that does not halt in time fails the test.

    Arguments:
      source (str):    The MISA assembly source to run.
      max_steps (int): The maximum number of instructions to simulate before giving up.

    Returns:
      Simulator: The halted simulator, ready for state inspection.
    """

    with tempfile.TemporaryDirectory() as workdir:
        bin_path: Path = assemble(source, workdir)
        sim: Simulator = Simulator()
        sim.load_mem(str(bin_path))

        steps: int = 0
        while (not sim.in_reset and steps < max_steps):
            sim.step()
            steps += 1
        if (steps >= max_steps):
            raise AssertionError(f"program did not halt within {max_steps} steps")
        return sim


def run_program(name: str, max_steps: int = 1000) -> Simulator:
    """
    Runs a named program fixture from the programs directory.

    Arguments:
      name (str):      The file name of the fixture in the programs directory, such as
                       "arithmetic.S".
      max_steps (int): The maximum number of instructions to simulate before giving up.

    Returns:
      Simulator: The halted simulator, ready for state inspection.
    """

    source: str = (PROGRAMS_DIR / name).read_text()
    return run_source(source, max_steps)
