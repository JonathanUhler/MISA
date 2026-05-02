#!/usr/bin/python3


"""
The command line interface wrapper for the misa assembler misa-as written in Haskell.

The main assembler code (which actually does the parsing and assembling to object files) can be
found in the misa-as subdirectory of this respository. It provides a very minimal command line
interface to assemble one .S file to one .o file at a time, which this script calls multiple times
to make for a better user experience.

See the help text (-h) for more information on usage.

Author: Jonathan Uhler
"""


from argparse import ArgumentParser, Namespace
import os
from pathlib import Path
import shlex
import subprocess
from subprocess import CompletedProcess
from tempfile import NamedTemporaryFile
from helpers import SmartHelpFormatter, error, format_subprocess_output, sort_paths


program_name: str = "misa-as"
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
    "All source files are run through the misa-pr preprocessor before assembling or linking. The "
    "-p flag can be specified to stop after the preprocessor and before the assembler."
    "\n\n"
    "Linking will happen automatically if the assembly of all source files is successful. The "
    "linker to invoke can be specified with the --linker option. Extra options can be passed to "
    "the linker with the --linker-options flag. misa-as will invoke the linker with the extra "
    "options as well as the paths of all generated object files and all non-source files. For "
    "instance:"
    "\n\n"
    "  misa-as file1.s file2.asm library.o \\"
    "\n"
    "    --linker /usr/bin/misa-ld         \\"
    "\n"
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


def generate_output_paths(src_paths: list) -> list:
    """
    From a list of source file paths, creates a list of output object file paths for if the -a
    option is specified.

    Each source file of the form <src_name>.<src_ext> for <src_ext> in {.asm, .s, .src} is taken
    and transformed into an object file of the form <src_name>.o

    Note that if multiple source files have the same <src_name> but different <src_ext>, they
    will map to the same object file name. This is intended behavior in line with the as assembler,
    and only the last source file assembled will be kept (previous ones will be overwritten). In
    this case, the returned list will still contain both object file names (which will be
    duplicates).

    Arguments:
      src_paths (list): The list of source file names.

    Returns:
      list: The list of object file names in the same order as the names in src_paths.
    """

    output_paths: list = []
    for src_path in src_paths:
        src_name: str = os.path.splitext(os.path.basename(src_path))[0]
        output_paths.append(f"{src_name}.o")
    return output_paths


def preprocess(src_path: Path, out_path: Path) -> (bool, str):
    result: CompletedProcess = \
        subprocess.run(["misa-pr", str(src_path), "-o", str(out_path)], capture_output = True)
    processed: bool = result.returncode == 0
    return processed, format_subprocess_output(result)


def preprocess_all(src_paths: list) -> list:
    processed_files: list = []

    for src_path in src_paths:
        out_file: NamedTemporaryFile = NamedTemporaryFile()
        out_path: Path = Path(out_file.name)
        processed, errors = preprocess(src_path, out_path)
        if (not processed):
            error(program_name, errors)
        processed_files.append(out_file)

    return processed_files


def assemble(src_path: Path, out_path: Path) -> (bool, str):
    """
    Calls the misa-as-exe Haskell assembler on a single source file.

    This function is a wrapper for the following invocation. The misa-as-exe binary must be
    in the users PATH.

      misa-as-exe ${src_path} ${out_path}

    Arguments:
      src_path (Path): Path to the assembly source file to assemble, which must exist.
      out_path (Path): Path to write the output object file, which will be overwritten if it exists.

    Returns:
      bool: True if the assembly was successful and out_path contains the object file contents.
            False if the assembly failed.
      str:  The stdout and stderr contents of misa-as-exe, which can be presented to the user if
            assembly failed.
    """

    result: CompletedProcess = \
        subprocess.run(["misa-as-exe", str(src_path), str(out_path)], capture_output = True)
    assembled: bool = result.returncode == 0
    return assembled, format_subprocess_output(result)


def assemble_all(src_paths: list) -> list:
    """
    Assembles a list of source files by calling assemble() on each one in order.

    See the assemble(str, str) function for more information. This function takes each src_path
    in src_paths, creates a temporary file for the output object file, and calls:

      assemble(src_path, temp_obj_file)

    The first assembler error will terminate this CLI wrapper.

    Arguments:
      src_paths (list): List of all source assembly files to assemble.

    Returns:
      list: A list of NamedTemporaryFile objects which each correspond to one of the assembled
            source files, in order.
    """

    artifact_files: list = []

    for src_path in src_paths:
        out_file: NamedTemporaryFile = NamedTemporaryFile()  # pylint:disable=consider-using-with
        out_path: Path = Path(out_file.name)
        assembled, errors = assemble(src_path, out_path)
        if (not assembled):
            error(program_name, errors)
        artifact_files.append(out_file)

    return artifact_files


def emit_processed_files(processed_files: list, output_path: str) -> None:
    if (output_path is not None and len(processed_files) > 1):
        error(program_name, "-o cannot be specified with -p and muliple source files")

    for processed_file in processed_files:
        if (output_path is None):
            print(processed_file.read().decode())
        else:
            with open(output_path, "wb+") as output_file:
                output_file.write(processed_file.read())


def emit_artifact_files(artifact_files: list, output_paths: list) -> None:
    """
    Moves all the NamedTemporaryFile object files from assemble_all into permanent .o files at
    the specified output paths.

    This function is intended to be used with the -a option, which preserves object files. The
    temporary artifact_files must still exist and be open/readable when this function is called.
    The same number of artifact files and output paths must be provided, and they will be mapped
    1-to-1.

    Arguments:
      artifact_files (list): List of temporary object files to save.
      output_paths (list):   List of output paths to write each temporary object file to.
    """

    if (len(artifact_files) != len(output_paths)):
        error(program_name, "-o cannot be specified with -a and more than one source file")

    for i, artifact_file in enumerate(artifact_files):
        output_path: str = output_paths[i]
        with open(output_path, "wb+") as output_file:
            output_file.write(artifact_file.read())


def emit_archive_file(artifact_files: list, output_path: str) -> None:
    result: CompletedProcess = \
        subprocess.run(["misa-ar", "c",
                        *[artifact_file.name for artifact_file in artifact_files],
                        "-o", output_path])
    if (result.returncode != 0):
        error(program_name, format_subprocess_output(result))


def link_artifact_files(artifact_files: list,
                        obj_paths: list,
                        linker_options: list,
                        output: str) -> None:
    """
    Links many assembled and provided object files together into a final executable.

    This function is a wrapper for the following linker invocation:

      misa-ld ${linker_options} ${artifact_files} ${obj_paths} [-o ${output}]

    Arguments:
      artifact_files (list): List of NamedTemporaryFiles for the objects generated by the assembled.
      obj_paths (list):      List of string paths to other files to pass to the linker as positional
                             arguments.
      linker_options (list): A list of arbitrary strings to pass to the linker as CLI flags.
      output (str):          String path/name to output the final executable to. If this is None,
                             no string will be passed (causing the linker to emit to whatever its
                             default path is).
    """

    artifact_options: str = " ".join([artifact_file.name for artifact_file in artifact_files])
    obj_options: str = " ".join([str(obj_path) for obj_path in obj_paths])
    output_option: str = "" if output is None else f"-o {output}"
    linker_option: str = " ".join(linker_options)
    linker_command: str = \
        f"misa-ld {linker_option} {artifact_options} {obj_options} {output_option}"

    result: CompletedProcess = subprocess.run(shlex.split(linker_command), capture_output = True)
    if (result.returncode != 0):
        error(program_name, f"linker messages\n{format_subprocess_output(result)}")


def main() -> None:
    """
    Command line entry point
    """

    parser: ArgumentParser = ArgumentParser(
        prog = program_name,
        formatter_class = SmartHelpFormatter,
        description = help_text
    )

    parser.add_argument("-a", "--assemble", action = "store_true",
                        help = "assemble but do not link")
    parser.add_argument("-p", "--preprocess", action = "store_true",
                        help = "preprocess but do not assemble or link")
    parser.add_argument("-l", "--linker-options",
                        action = "append", metavar = "<options>", default = [],
                        help = "string of command-line options to pass to misa-ld")
    parser.add_argument("-o", "--output", metavar = "<file>",
                        help = "place the output into <file>")
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    parser.add_argument("asmfile", nargs = "+", type = Path, metavar = "<asmfile ...>",
                        help = "assemble source files <asmfile ...>")
    args: Namespace = parser.parse_args()

    src_paths, obj_paths = sort_paths(args.asmfile, {".asm", ".s", ".src"})
    processed_files: list = preprocess_all(src_paths)
    if (args.preprocess):
        emit_processed_files(processed_files, args.output)
        return

    processed_paths: list = [processed_file.name for processed_file in processed_files]
    artifact_files: list = assemble_all(processed_paths)
    if (args.assemble):
        if (len(artifact_files) > 1 and args.output is not None):
            emit_archive_file(artifact_files, args.output)
        else:
            output_paths: list = \
                [args.output] if args.output is not None else generate_output_paths(src_paths)
            emit_artifact_files(artifact_files, output_paths)
        return

    link_artifact_files(artifact_files,
                        obj_paths,
                        linker_options = args.linker_options,
                        output = args.output)


if (__name__ == "__main__"):
    main()
