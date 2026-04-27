#!/usr/bin/python3


from argparse import ArgumentParser, Namespace
import os
from pathlib import Path
import subprocess
from subprocess import CompletedProcess
from tempfile import TemporaryDirectory
from helpers import (SmartHelpFormatter,
                     error,
                     format_subprocess_output,
                     sort_paths,
                     extract_archives)


program_name: str = "misa-nm"
version_text: str = "0.1.0.0"
help_text: str = (
    "misa-nm lists the symbols from MISA_LF object files and MISA_AR archive files. If no files "
    "are listed as arguments, misa-nm will produce no output."
    "\n\n"
    "Archive files are identified by the case-insensitive extension '.a'. All other files provided "
    "as arguments are assumbed to be single MISA_LF object files. Archives will be extracted with "
    "the misa-ar archiver and the symbols in each archived object file will be printed as normal."
    "\n\n"
    "Symbols are grouped by the object file they are referenced in. For each symbol, misa-nm shows "
    "the following in the standard output:"
    "\n\n"
    "• The symbol type, indicating whether the symbol is defined in or required by the object file "
    "during linking:"
    "\n\n"
    "    <-  The symbol is defined in and exported by the object file."
    "\n"
    "    ->  The symbol is imported and required by the object file."
    "\n\n"
    "• The symbol or relocation address within the object file, in hexadeicmal."
    "\n"
    "• The name of the symbol as specified in the object file."
    "\n"
    "• If the symbol is required (->), whether the relocation address is the high or low word of "
    "the symbol's value (used for 16-bit symbols):"
    "\n\n"
    "    (hi)  The relocation will use the bits 15:8 of the symbol."
    "\n"
    "    (lo)  The relocation will use the bits 7:0 of the symbol. (Default for 8-bit symbols.)"
)


def name_archive(archive_path: Path) -> (bool, str):
    extract_dir: TemporaryDirectory = TemporaryDirectory()
    extracted, errors = extract_archives([archive_path], extract_dir)
    extracted_paths: list = [os.path.join(extract_dir.name, extracted_file)
                             for extracted_file in os.listdir(extract_dir.name)]
    if (not extracted):
        return extracted, errors

    all_output: list = []
    for obj_path in extracted_paths:
        named, output = name_object(obj_path)
        if (not named):
            return named, output
        all_output.append(f"Archive \"{archive_path}\": {output}")
    return True, "\n\n".join(all_output)


def name_object(obj_path: Path) -> (bool, str):
    result: CompletedProcess = subprocess.run(["misa-nm-exe", str(obj_path)], capture_output = True)
    named: bool = result.returncode == 0
    return named, format_subprocess_output(result)


def main() -> None:
    """
    Command line entry point for misa-nm.
    """

    parser: ArgumentParser = ArgumentParser(
        prog = program_name,
        formatter_class = SmartHelpFormatter,
        description = help_text
    )
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    parser.add_argument("objfile", nargs = "*", type = Path, metavar = "<objfile ...>",
                        help = "prints symbols used in object/archive files <objfile ...>")

    args: Namespace = parser.parse_args()

    archive_paths, _ = sort_paths(args.objfile, {".a"})
    for path in args.objfile:
        named, output = True, None
        if (path in archive_paths):
            named, output = name_archive(path)
        else:
            named, output = name_object(path)

        if (not named):
            error(program_name, output)
        else:
            print(output)


if (__name__ == "__main__"):
    main()
