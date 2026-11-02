from argparse import HelpFormatter
import os
import subprocess
from subprocess import CompletedProcess
import sys
from tempfile import TemporaryDirectory
import textwrap


class SmartHelpFormatter(HelpFormatter):

    def _fill_text(self, text: str, width: int, indent: int):
        lines: list = text.splitlines()
        wrapped: list = [
            textwrap.fill(line, width, initial_indent = indent, subsequent_indent = indent)
            for line in lines
        ]
        return "\n".join(wrapped)


def error(program: str, message: str) -> None:
    print(f"{program}: error: {message}")
    sys.exit(1)


def format_subprocess_output(process: CompletedProcess) -> str:
    output: str = ""

    has_stdout: bool = len(process.stdout) > 0 if process.stdout is not None else False
    has_stderr: bool = len(process.stderr) > 0 if process.stderr is not None else False

    if (has_stdout):
        output += process.stdout.decode("utf-8")
        if (has_stderr):
            output += "\n"
    if (has_stderr):
        output += process.stderr.decode("utf-8")

    return output


def sort_paths(paths: list, extensions: set) -> (list, list):
    matches: list = []
    nonmatches: list = []

    for path in paths:
        extension: str = os.path.splitext(path)[1]
        if (extension.lower() in extensions):
            matches.append(path)
        else:
            nonmatches.append(path)

    return matches, nonmatches


def extract_archives(archive_paths: list, extract_dir: TemporaryDirectory) -> (bool, str):
    for archive_path in archive_paths:
        result: CompletedProcess = \
            subprocess.run(["misa-ar", "x", archive_path, "-o", extract_dir.name])
        extracted: bool = result.returncode == 0
        if (not extracted):
            return False, format_subprocess_output(result)
    return True, ""


def get_extn_bits(extensions: list) -> int:
    """
    Converts a list of extension names from the -e option to the integer value of the EXTNS register
    that enables those specific extensions.

    Args:
      extensions (list): List of string extension names from the -e option.

    Returns:
      int: EXTNS register value for the enabled extensions.
    """

    extn_bits: int = 0
    if ("dynamichw" in extensions):
        extn_bits |= 0x0001
    if ("syscall" in extensions):
        extn_bits |= 0x0002
    if ("privilege" in extensions):
        extn_bits |= 0x0004
    if ("interrupt" in extensions):
        extn_bits |= 0x0008
    return extn_bits
