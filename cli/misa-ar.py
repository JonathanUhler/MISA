#!/usr/bin/python3


"""
The command line interface wrapper for the misa archiver misa-ar written in Haskell.

The archiver functionality can be found int he misa-ar subdirectory of this repository. The Haskell
code has a very basic CLI, which this script extends with a prettier one.

See the help text (-h) for more information on usage.

Author: Jonathan Uhler
"""


from argparse import ArgumentParser, Namespace
from pathlib import Path
import subprocess
from subprocess import CompletedProcess
from helpers import SmartHelpFormatter, error, format_subprocess_output


program_name: str = "misa-ar"
version_text: str = "0.1.0.0"
help_text: str = (
    "misa-ar is an archiver tool for object files in the MISA linkable format. It takes one or "
    "more object files produced by the misa-as assembler (run with -a) and combines them into a "
    "single file that preserves the contents of the object files for later use."
    "\n\n"
    "misa-ar is intended for use in generating libraries made of many object files that can be "
    "passed to and automatically extracted by the misa-ld linker. Only the object file contents "
    "are preserved by misa-ar, and other filesystem metadata may not be restored on extraction."
    "\n\n"
    "misa-ar performs different actions based on the specified sub-command. The following are "
    "mutually-exclusive actions that can be performed:"
    "\n\n"
    "  c\tCreate a new archive from one or more object files."
    "\n"
    "  x\tExtract the files within an archive."
)


def create(obj_paths: list, out_path: Path) -> (bool, str):
    result: CompletedProcess = \
        subprocess.run(["misa-ar-exe", "c", *obj_paths, str(out_path)], capture_output = True)
    created: bool = result.returncode == 0
    return created, format_subprocess_output(result)


def extract(archive_path: Path, out_path: Path) -> (bool, str):
    result: CompletedProcess = \
        subprocess.run(["misa-ar-exe", "x", str(archive_path), str(out_path)],
                       capture_output = True)
    extracted: bool = result.returncode == 0
    return extracted, format_subprocess_output(result)


def main() -> None:
    """
    Command line entry point for misa-ar.
    """

    parser: ArgumentParser = ArgumentParser(
        prog = program_name,
        formatter_class = SmartHelpFormatter,
        description = help_text
    )
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    subparsers: list = parser.add_subparsers(dest = "command", required = True)

    create_parser: ArgumentParser = subparsers.add_parser("c")
    create_parser.add_argument("-o", "--output", metavar = "<file>", default = "library.a",
                               help = "place the output into <file>")
    create_parser.add_argument("objfile", nargs = "+", type = Path, metavar = "<objfile ...>",
                               help = "archive object files <objfile ...>")

    extract_parser: ArgumentParser = subparsers.add_parser("x")
    extract_parser.add_argument("-o", "--output", metavar = "<dir>", default = ".",
                                help = "place the extracted files into <dir>")
    extract_parser.add_argument("archive", type = Path, metavar = "<archive>",
                                help = "extract files from <archive>")

    args: Namespace = parser.parse_args()

    finished, errors = False, None
    if (args.command == "c"):
        finished, errors = create(args.objfile, args.output)
    elif (args.command == "x"):
        finished, errors = extract(args.archive, args.output)
    else:
        error(program_name, f"unsupported action '{args.command}'")

    if (not finished):
        error(program_name, errors)


if (__name__ == "__main__"):
    main()
