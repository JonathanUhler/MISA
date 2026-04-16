#!/usr/bin/python3


from argparse import ArgumentParser, HelpFormatter, Namespace
import os
from pathlib import Path
import shlex
import shutil
import subprocess
from subprocess import CompletedProcess
import sys
from tempfile import NamedTemporaryFile
import textwrap


default_memmap: str = \
"""
  section 0x0000 - 0x00FF : direct ;       // Zero pages
  section 0x0100 - 0X01FF : ;              // Stack
  section 0x0200 - 0x7FFF : data bss ;     // RAM
  section 0x8000 - 0xBFFF : ;              // Memory-mapped IO
  section 0xC000 - 0xFFF0 : text rodata ;  // Program ROM
  section 0xFFFA - 0xFFFF : vectors ;      // Hardware vector addresses
"""


version_text: str = "0.1.0.0"


help_text: str = (
    "misa-ld is the linker for the MISA linkable format of object files. It takes the object "
    "files produced by the misa-as assembler (run with -a) and combines them into a single flat "
    "binary, handling symbol placement and relocation."
    "\n\n"
    "misa-ld accepts memory map files (a basic form of linker scripts) that explain to the "
    "linker how to place sections of the program binary within the address space. The basic "
    "grammar of the memory map file is:"
    "\n\n"
    "  memmap  ::= (\"section\" number \"-\" number \":\" (section)* \";\")+"
    "\n"
    "  number  ::= 0x(0-9a-fA-F)+"
    "\n"
    "  section ::= (_a-zA-Z)(_a-zA-Z0-9)*"
    "\n\n"
    "If a memory map file is not specified with the -M switch, the following default one will be "
    "used:"
    "\n\n"
    f"{default_memmap}"
    "\n\n"
    "misa-ld will always produce a flat binary, and not a new linkable object file. The binary "
    "represents the entire 64kB address space allowed by the MISA instruction set architecture. "
    "This file can be loaded directly into memory of a MISA processor to run."
)


class SmartHelpFormatter(HelpFormatter):

    def _fill_text(self, text: str, width: int, indent: int):
        lines: list = text.splitlines()
        wrapped: list = [
            textwrap.fill(line, width,  initial_indent = indent, subsequent_indent = indent)
            for line in lines
        ]
        return "\n".join(wrapped)


def error(message: str) -> None:
    print(f"misa-ld: error: {message}")
    sys.exit(1)


def format_subprocess_output(process: CompletedProcess) -> str:
    output: str = ""

    has_stdout: bool = len(process.stdout) > 0
    has_stderr: bool = len(process.stderr) > 0

    if (has_stdout):
        output += process.stdout.decode("utf-8")
        if (has_stderr):
            output += "\n"
    if (has_stderr):
        output += process.stderr.decode("utf-8")

    return output


def get_default_memmap() -> NamedTemporaryFile:
    memmap_file: NamedTemporaryFile = NamedTemporaryFile()
    memmap_file.write(default_memmap.encode())
    memmap_file.seek(0)
    return memmap_file


def link(memmap_path: Path, obj_paths: list, out_path: Path) -> (bool, str):
    result: CompletedProcess = \
        subprocess.run(["misa-ld-exe", str(memmap_path), *obj_paths, str(out_path)],
                       capture_output = True)
    linked: bool = result.returncode == 0
    return linked, format_subprocess_output(result)


def main() -> None:
    parser: ArgumentParser = ArgumentParser(
        prog = "misa-ld",
        formatter_class = SmartHelpFormatter,
        description = help_text
    )

    parser.add_argument("-o", "--output", metavar = "<file>", default = "memory.bin",
                        help = "place the output into <file>")
    parser.add_argument("-M", "--memmap", metavar = "<path>",
                        help = "use the memory map file at <path>")
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    parser.add_argument("objfile", nargs = "+", type = Path, metavar = "<objfile ...>",
                        help = "link object files <objfile ...>")
    args: Namespace = parser.parse_args()

    memmap_path: Path = args.memmap
    if (memmap_path is None):
        memmap_file: NamedTemporaryFile = get_default_memmap()
        memmap_path: str = Path(memmap_file.name)

    linked, errors = link(memmap_path, args.objfile, args.output)
    if (not linked):
        error(errors)


if (__name__ == "__main__"):
    main()
