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


help_text: str = (
    "misa-as is the assembler for the MISA instruction set architecture. It is intended to "
    "assemble source assembly language files into object files for use by the misa-ld linker."
    "\n\n"
    "Each time misa-as runs, it assembles exactly one object file per source file. Source files "
    "are those which end with one of the following case-insensitive extensions: .asm, .s, .src. "
    "Files provided to misa-as without these extensions are assumed to be pre-assembled object "
    "files or libraries and will be passed directly to the linker (if invoked)."
    "\n\n"
    "Linking will happen automatically if the assembly of all source files is successful. The "
    "linker to invoke can be specified with the --linker option. Extra options can be passed to "
    "the linker with the --linker-options flag. misa-as will invoke the linker with the extra "
    "options as well as the paths of all generated object files and all non-source files. For "
    "instance:"
    "\n\n"
    "  misa-as file1.s file2.asm library.o \\"
    "    --linker /usr/bin/misa-ld         \\"
    "    --linker-options \"-M memmap.ld other.o\""
    "\n\n"
    "Will result in this call to the linker:"
    "\n\n"
    "  /usr/bin/misa-ld -M memmap.ld other.o file1.o file2.o library.o"
    "\n\n"
    "The linker will not be invoked when the -a flag is specified. In this case, only source "
    "files will be assembled, generated object files will not be deleted, and non-source files "
    "provided to misa-as will be ignored. The -o flag cannot be specified with the -a flag when "
    "more than one source file is provided."
)


class SmartHelpFormatter(HelpFormatter):

    def _fill_text(self, text: str, width: int, indent: int):
        lines: list = text.splitlines()
        wrapped: list = [
            textwrap.fill(line, width, initial_indent = indent, subsequent_indent = indent)
            for line in lines
        ]
        return "\n".join(wrapped)


def error(message: str) -> None:
    print(f"misa-as: error: {message}")
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


def sort_paths(asm_paths: list) -> (list, list):
    src_extensions: set = {".asm", ".s", ".src"}

    src_paths: list = []
    obj_paths: list = []

    for asm_path in asm_paths:
        extension: str = os.path.splitext(asm_path)[1]
        if (extension.lower() in src_extensions):
            src_paths.append(asm_path)
        else:
            obj_paths.append(asm_path)

    return src_paths, obj_paths


def generate_output_paths(src_paths: list) -> list:
    output_paths: list = []
    for src_path in src_paths:
        src_name: str = os.path.splitext(os.path.basename(src_path))[0]
        output_paths.append(f"{src_name}.o")
    return output_paths


def assemble(src_path: Path, out_path: Path) -> (bool, str):
    result: CompletedProcess = \
        subprocess.run(["misa-as-exe", str(src_path), str(out_path)], capture_output = True)
    assembled: bool = result.returncode == 0
    return assembled, format_subprocess_output(result)


def assemble_all(src_paths: list) -> list:
    artifact_files: list = []

    for src_path in src_paths:
        out_file: file = NamedTemporaryFile()
        out_path: Path = Path(out_file.name)
        assembled, errors = assemble(src_path, out_path)
        if (not assembled):
            error(errors)
        artifact_files.append(out_file)

    return artifact_files


def emit_artifact_files(artifact_files: list, output_paths: list) -> None:
    if (len(artifact_files) != len(output_paths)):
        error("-o cannot be specified with -a and more than one source file")

    for i, artifact_file in enumerate(artifact_files):
        output_path: str = output_paths[i]
        with open(output_path, "wb+") as output_file:
            output_file.write(artifact_file.read())


def link_artifact_files(artifact_files: list,
                        obj_paths: list,
                        linker: str,
                        linker_options: list,
                        output: str) -> None:
    artifact_options: str = " ".join([artifact_file.name for artifact_file in artifact_files])
    obj_options: str = " ".join([str(obj_path) for obj_path in obj_paths])
    output_option: str = "" if output is None else f"-o {output}"
    linker_option: str = " ".join(linker_options)
    linker_command: str = \
        f"{linker} {linker_option} {artifact_options} {obj_options} {output_option}"

    result: CompletedProcess = subprocess.run(shlex.split(linker_command), capture_output = True)
    if (result.returncode != 0):
        error(f"linker messages\n{format_subprocess_output(result)}")


def main() -> None:
    parser: ArgumentParser = ArgumentParser(
        prog = "misa-as",
        formatter_class = SmartHelpFormatter,
        description = help_text
    )

    parser.add_argument("-a", "--assemble", action = "store_true",
                        help = "assemble but do not link")
    parser.add_argument("--linker", metavar = "<linker>", default = "misa-ld",
                        help = "name or path or the linker binary to invoke")
    parser.add_argument("--linker-options", action = "append", metavar = "<options>", default = [],
                        help = "string of command-line options to pass to <linker>")
    parser.add_argument("-o", "--output", metavar = "<file>",
                        help = "place the output into <file>")
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    parser.add_argument("asmfile", nargs = "+", type = Path, metavar = "<asmfile ...>",
                        help = "assemble source files <asmfile ...>")
    args: Namespace = parser.parse_args()

    src_paths, obj_paths = sort_paths(args.asmfile)
    artifact_files: list = assemble_all(src_paths)

    if (args.assemble):
        if (len(artifact_files) > 1 and args.output is not None):
            error("-o cannot be specified with -a and more than one source file")
        output_paths: list = \
            [args.output] if args.output is not None else generate_output_paths(src_paths)
        emit_artifact_files(artifact_files, output_paths)
    else:
        link_artifact_files(artifact_files,
                            obj_paths,
                            linker = args.linker,
                            linker_options = args.linker_options,
                            output = args.output)


if (__name__ == "__main__"):
    main()
