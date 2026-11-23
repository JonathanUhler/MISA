#!/usr/bin/python3


"""
The command line interface wrapper for the misa functional simulator misa-spec written in Lean4.

The simulator implementation can be found in the misa-spec subdirectory of this repository. The
Lean code has a very basic CLI, which this script extends with a prettier one.

See the help text (-h) for more information on usage.

Author: Jonathan Uhler
"""


from argparse import ArgumentParser, Namespace
from pathlib import Path
import subprocess
from subprocess import CompletedProcess
from helpers import SmartHelpFormatter, error, format_subprocess_output, get_extn_bits


program_name: str = "misa-spec"
version_text: str = "0.1.0.0"
help_text: str = (
    "misa-spec is a semantically-correct simulator of the MISA instruction set architecture, "
    "including all its architectural extensions. It defines a deterministic finite automata (MISA "
    "has finite memory, so is not formally Turing-complete) that takes states to states based on "
    "the instruction word at the program counter. The state space is combinations of: "
    "\n\n"
    "• General purpose registers"
    "\n"
    "• Special registers"
    "\n"
    "• Addressable memory"
    "\n"
    "• Program counter"
    "\n"
    "• Shadow state (in a syscall, in an interrupt, saved interrupt registers)"
    "\n"
    "• Halted"
    "\n\n"
    "misa-spec can be run from the canonical initial state, q₀, or from an arbitrary state. After "
    "the state is specified, the simulator will run for a specified number of steps of the state "
    "transition function, or until the simulator halts. The first command line argument determines "
    "which mode is used:"
    "\n\n"
    "  q0\tRun from the canonical initial state."
    "\n"
    "  qi\tRun from an arbitrary state defined by a YAML-like file."
    "\n"
    "  sweep\tRun many patched copies of one memory file, one after another."
    "\n\n"
    "In q0 and sweep mode, the memory the simulated implementation provides can be described with "
    "a memory map file passed with the -M option. This is the same file format that the misa-ld "
    "linker uses. When -M is not specified, the simulator provides the entire 16-bit address space "
    "and permits every kind of access to any address."
    "\n\n"
    "By default only the state the run ends in is reported. The -t option instead reports one "
    "record per state transition. Sweep mode always reports one record per job. In both cases the "
    "first record is complete and every later record lists only what changes from the previous "
    "record."
)


def steps_type(steps_str: str) -> str:
    if (steps_str == "-"):
        return steps_str
    try:
        _ = int(steps_str, 0)
        return steps_str
    except ValueError as e:
        raise ValueError("step count is not an integer literal or '-'") from e


def format_interrupts(steps: list) -> str:
    if (not steps):
        return "-"
    return ",".join(str(step) for step in steps)


def trace_args(trace: bool) -> list:
    return ["trace"] if trace else []


def run_from_q0(mem_path: Path, steps: str, extns: int, ints: str, map_path: Path,
                trace: bool) -> (bool, str):
    result: CompletedProcess = subprocess.run(
        ["misa-spec-exe", "q0", mem_path, steps, str(extns), ints, map_path] + trace_args(trace),
        capture_output = True
    )
    ran: bool = result.returncode == 0
    return ran, format_subprocess_output(result)


def run_from_qi(state_path: Path, steps: str, ints: str, trace: bool) -> (bool, str):
    result: CompletedProcess = subprocess.run(
        ["misa-spec-exe", "qi", state_path, steps, ints] + trace_args(trace),
        capture_output = True
    )
    ran: bool = result.returncode == 0
    return ran, format_subprocess_output(result)


def run_sweep(mem_path: Path, steps: str, extns: int, ints: str, map_path: Path,
              jobs_path: Path) -> (bool, str):
    result: CompletedProcess = subprocess.run(
        ["misa-spec-exe", "sweep", mem_path, steps, str(extns), ints, map_path, jobs_path],
        capture_output = True
    )
    ran: bool = result.returncode == 0
    return ran, format_subprocess_output(result)


def main() -> None:
    """
    Command line entry point for misa-spec.
    """

    parser: ArgumentParser = ArgumentParser(
        prog = program_name,
        formatter_class = SmartHelpFormatter,
        description = help_text
    )
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    subparsers: list = parser.add_subparsers(dest = "command", required = True)

    q0_parser: ArgumentParser = subparsers.add_parser("q0")
    q0_parser.add_argument("-e", "--extension", nargs = "*", metavar = "<extension>", default = [],
                           choices = {"dynamichw", "syscall", "privilege", "interrupt"},
                           help = "enable architecture extension <extension> in the simulator")
    q0_parser.add_argument("-i", "--interrupt", nargs = "*", metavar = "<step>", default = [],
                           type = int,
                           help = "assert the interrupt pin before each state transition <step>")
    q0_parser.add_argument("-n", "--steps", type = steps_type, metavar = "<n>", default = "-",
                           help = "run for maximum <n> state transitions (default: until halt)")
    q0_parser.add_argument("-o", "--output", metavar = "<file>",
                           help = "write the final state information to <output>")
    q0_parser.add_argument("-M", "--memmap", metavar = "<memmap>",
                           help = "use the memory map file at <memmap>")
    q0_parser.add_argument("-t", "--trace", action = "store_true",
                           help = "report one record per state transition, not just the last state")
    q0_parser.add_argument("memfile", type = Path, metavar = "<memfile>",
                           help = "initialize simulator memory from binary file <memfile>")

    qi_parser: ArgumentParser = subparsers.add_parser("qi")
    qi_parser.add_argument("-i", "--interrupt", nargs = "*", metavar = "<step>", default = [],
                           type = int,
                           help = "assert the interrupt pin before each state transition <step>")
    qi_parser.add_argument("-n", "--steps", type = steps_type, metavar = "<n>", default = "-",
                           help = "run for maximum <n> state transitions (default: until halt)")
    qi_parser.add_argument("-o", "--output", metavar = "<file>",
                           help = "write the final state information to <output>")
    qi_parser.add_argument("-t", "--trace", action = "store_true",
                           help = "report one record per state transition, not just the last state")
    qi_parser.add_argument("statefile", type = Path, metavar = "<statefile>",
                           help = "initialize simulator from YAML-like state file <statefile>")

    sweep_parser: ArgumentParser = subparsers.add_parser("sweep")
    sweep_parser.add_argument("-e", "--extension", nargs = "*", metavar = "<extension>",
                              default = [],
                              choices = {"dynamichw", "syscall", "privilege", "interrupt"},
                              help = "enable architecture extension <extension> in the simulator")
    sweep_parser.add_argument("-i", "--interrupt", nargs = "*", metavar = "<step>", default = [],
                              type = int,
                              help = "assert the interrupt pin before each state transition <step>")
    sweep_parser.add_argument("-n", "--steps", type = steps_type, metavar = "<n>", default = "-",
                              help = "run each job for maximum <n> state transitions "
                                     "(default: until halt)")
    sweep_parser.add_argument("-o", "--output", metavar = "<file>",
                              help = "write the state information to <output>")
    sweep_parser.add_argument("-M", "--memmap", metavar = "<memmap>",
                              help = "use the memory map file at <memmap>")
    sweep_parser.add_argument("memfile", type = Path, metavar = "<memfile>",
                              help = "initialize simulator memory from binary file <memfile>")
    sweep_parser.add_argument("jobfile", type = Path, metavar = "<jobfile>",
                              help = "run one job per line of <jobfile>")

    args: Namespace = parser.parse_args()

    finished, output = False, ""
    if (args.command == "q0"):
        map_path: str = "-" if (args.memmap is None) else str(args.memmap)
        finished, output = run_from_q0(
            args.memfile,
            args.steps,
            get_extn_bits(args.extension),
            format_interrupts(args.interrupt),
            map_path,
            args.trace
        )
    elif (args.command == "qi"):
        finished, output = run_from_qi(
            args.statefile,
            args.steps,
            format_interrupts(args.interrupt),
            args.trace
        )
    elif (args.command == "sweep"):
        map_path = "-" if (args.memmap is None) else str(args.memmap)
        finished, output = run_sweep(
            args.memfile,
            args.steps,
            get_extn_bits(args.extension),
            format_interrupts(args.interrupt),
            map_path,
            args.jobfile
        )
    else:
        error(program_name, f"unsupported action '{args.command}'")

    if (not finished):
        error(program_name, output)

    if (args.output):
        with open(args.output, "w+", encoding = "utf-8") as f:
            f.write(output)
    else:
        print(output)


if (__name__ == "__main__"):
    main()
