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


from argparse import ArgumentParser, HelpFormatter, Namespace
import os
from pathlib import Path
import shlex
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


class SmartHelpFormatter(HelpFormatter):
    """
    An argparse formatter help text than preserves explicit newlines in the original help text,
    but still wraps lines within the same sentence (split by \n).
    """

    def _fill_text(self, text: str, width: int, indent: int):
        lines: list = text.splitlines()
        wrapped: list = [
            textwrap.fill(line, width, initial_indent = indent, subsequent_indent = indent)
            for line in lines
        ]
        return "\n".join(wrapped)


def error(message: str, retcode: int = 1) -> None:
    """
    Produces an error message from misa-as and exits with a failure return code.

    Arguments:
      message (str): The message to print to standard output, which will be prefixed with the
                     extra string "misa-as: error: "
      retcode (int): The return code to exit with.
    """

    print(f"misa-as: error: {message}")
    sys.exit(retcode)


def format_subprocess_output(process: CompletedProcess) -> str:
    """
    Retreives and formats the output of a subprocess CompletedProcess object.

    This method checks if stdout and stderr are present. If they are, they are added to the
    formatted output returned by this function in that order. If only one is present, only it will
    be added. If both are present, they are separated by a single newline. If both streams are
    empty, an empty string is returned.

    Arguments:
      process (CompletedProcess): The result of a subprocess running.

    Returns:
      str: The formatted output of the subprocess, if present.
    """

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
    """
    Sorts a list of paths on the command line into assembly source files and object files based
    on their file extensions.

    Files that have the following case-insensitive extensions will be considered to be assembly
    source files: .asm, .s, .src. All other files are considered to be object files and will be
    passed to the linker without being assembled.

    Arguments:
      asm_paths (list): The list of all file paths from the command line, in any order.

    Returns:
      list: The list of files in the order they appear in asm_paths which are source files.
      list: The list of files in the order they appear in asm_paths which are NOT source files.
    """

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
            error(errors)
        artifact_files.append(out_file)

    return artifact_files


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
    """
    Links many assembled and provided object files together into a final executable.

    This function is a wrapper for the following linker invocation:

      ${linker} ${linker_options} ${artifact_files} ${obj_paths} [-o ${output}]

    Arguments:
      artifact_files (list): List of NamedTemporaryFiles for the objects generated by the assembled.
      obj_paths (list):      List of string paths to other files to pass to the linker as positional
                             arguments.
      linker (str):          The name or path of the linker executable to invoke.
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
        f"{linker} {linker_option} {artifact_options} {obj_options} {output_option}"

    result: CompletedProcess = subprocess.run(shlex.split(linker_command), capture_output = True)
    if (result.returncode != 0):
        error(f"linker messages\n{format_subprocess_output(result)}")


def main() -> None:
    """
    Command line entry point
    """

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
