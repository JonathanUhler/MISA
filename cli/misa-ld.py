#!/usr/bin/python3


"""
The command line interface wrapper for the misa-ld linker.

This script exists to provide a higher quality interface to the Haskell linker, which only supports
a very minimal CLI to link several files together with a provided memory map file.

See the help text for more usage information.

Author: Jonathan Uhler
"""


from argparse import ArgumentParser, Namespace
from pathlib import Path
import subprocess
from subprocess import CompletedProcess
from tempfile import NamedTemporaryFile
from helpers import SmartHelpFormatter, error, format_subprocess_output


default_memmap: str = \
"""
  section 0x0000 - 0x00FF : direct      ;  // Zero pages
  section 0x0100 - 0X01FF :             ;  // Stack
  section 0x0200 - 0x7FFF : data bss    ;  // RAM
  section 0x8000 - 0xBFFF :             ;  // Memory-mapped IO
  section 0xC000 - 0xFFEF : text rodata ;  // Program ROM
  section 0xFFF0 - 0xFFFF : reset       ;  // Reset code
"""


program_name: str = "misa-ld"
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


def get_default_memmap() -> NamedTemporaryFile:
    """
    Writes the default memory map in the default_memmap global string to a temporary file and
    returns that NamedTemporaryFile pointer.

    Returns:
      NamedTemporaryFile: The temporary file containing the default memory map.
    """

    memmap_file: NamedTemporaryFile = NamedTemporaryFile()
    memmap_file.write(default_memmap.encode())
    memmap_file.seek(0)
    return memmap_file


def link(memmap_path: Path, obj_paths: list, out_path: Path) -> (bool, str):
    """
    Links several object files together with a memory map file to produce a final executable.

    This function is a wrapper for the following Haskell linker call, which must be in the user's
    PATH:

      misa-ld-exe ${memmap_path}, ${obj_paths} ${out_path}

    Arguments:
      memmap_path (Path): Path to the memory map file to use. This can be a temporary file obtained
                          by creating the default memmap.
      obj_paths (list):   A list of Path objects to the object files that are being linked.
      out_path (Path):    Path to write the output flat binary.

    Returns:
      bool: True if linking was successful, False if any error occured. out_path will only be
            usable as the final binary if this is True.
      str:  The stdout and stderr from the Haskell linker, which can be presented to the user as
            debug information if linking failed.
    """

    result: CompletedProcess = \
        subprocess.run(["misa-ld-exe", str(memmap_path), *obj_paths, str(out_path)],
                       capture_output = True)
    linked: bool = result.returncode == 0
    return linked, format_subprocess_output(result)


def main() -> None:
    """
    Command line entry point for misa-ld.
    """

    parser: ArgumentParser = ArgumentParser(
        prog = program_name,
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
        error(program_name, errors)


if (__name__ == "__main__"):
    main()
