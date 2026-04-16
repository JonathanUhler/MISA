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


version_text: str = "0.1.0.0"


help_text: str = \
"""
misa-ld
"""


default_script: str = \
"""
SECTION 0x0000 - 0x00FF : direct ;       // Zero pages
SECTION 0x0100 - 0X01FF : ;              // Stack
SECTION 0x0200 - 0x7FFF : data bss ;     // RAM
SECTION 0x8000 - 0xBFFF : ;              // Memory-mapped IO
SECTION 0xC000 - 0xFFF0 : text rodata ;  // Program ROM
SECTION 0xFFFA - 0xFFFF : vectors ;      // Hardware vector addresses
"""


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


def get_default_script() -> NamedTemporaryFile:
    script_file: NamedTemporaryFile = NamedTemporaryFile()
    script_file.write(default_script.encode())
    script_file.seek(0)
    return script_file


def link(script_path: Path, obj_paths: list, out_path: Path) -> (bool, str):
    result: CompletedProcess = \
        subprocess.run(["misa-ld-exe", str(script_path), *obj_paths, str(out_path)],
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
    parser.add_argument("-T", "--script", metavar = "<path>",
                        help = "use the linker script at <path>")
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    parser.add_argument("objfile", nargs = "+", type = Path, metavar = "<objfile ...>",
                        help = "link object files <objfile ...>")
    args: Namespace = parser.parse_args()

    script_path: Path = args.script
    if (script_path is None):
        script_file: NamedTemporaryFile = get_default_script()
        script_path: str = Path(script_file.name)

    linked, errors = link(script_path, args.objfile, args.output)
    if (not linked):
        error(errors)


if (__name__ == "__main__"):
    main()
