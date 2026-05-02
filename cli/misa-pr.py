#!/usr/bin/python3


from argparse import ArgumentParser, Namespace
from pathlib import Path
import subprocess
from subprocess import CompletedProcess
from helpers import (SmartHelpFormatter, error, format_subprocess_output)


program_name: str = "misa-pr"
version_text: str = "0.1.0.0"
help_text: str = (
    "misa-pr is the source-agnostic preprocessor intended for MISA assembly code. The preprocessor "
    "only cares about certain directives and syntax, and thus can be used on any type of source "
    "file content, not just assembly."
    "\n\n"
    "Files are preprocessed by analyzing any # directives they contain, acting on those "
    "directives, and emitting a processed output file. Files which have no directives (including "
    "bogus, non-source-code files) will be emitted as output with no changes."
    "\n\n"
    "Preprocessing has the following steps for each file being processed:"
    "\n\n"
    "• #include directives are extracted. For each #include, the included file is opened and any "
    "inclusions within it are recursively resolved. The final included content is pasted "
    "directly into the original file's contents where the original #include directive was."
    "\n"
    "• #macro directives are extracted from the combined original and included content. Note that "
    "macros are not expanded during the inclusion pass. Macros are expanded from top to bottom "
    "in the content being processed. Definitions of the same macro are allowed, and the macro "
    "value used will be the one defined most recently."
    "\n\n"
    "The emitted file will not contain any preprocessor directives, even if one or more "
    "directives were not used (e.g. superfluous macros)."
)


def preprocess(src_path: Path, output_path: str) -> None:
    result: CompletedProcess = subprocess.run(["misa-pr-exe", str(src_path), output_path])
    if (result.returncode != 0):
        error(program_name, format_subprocess_output(result))


def main() -> None:
    """
    Command line entry point for misa-pr.
    """

    parser: ArgumentParser = ArgumentParser(
        prog = program_name,
        formatter_class = SmartHelpFormatter,
        description = help_text
    )
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    parser.add_argument("-o", "--output", metavar = "<file>", default = "out.s",
                        help = "place the output into <file>")
    parser.add_argument("srcfile", type = Path, metavar = "<srcfile>",
                        help = "preprocess source file <srcfile>")

    args: Namespace = parser.parse_args()

    preprocess(args.srcfile, args.output)


if (__name__ == "__main__"):
    main()
