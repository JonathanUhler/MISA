from argparse import HelpFormatter
from subprocess import CompletedProcess
import sys
import textwrap


class SmartHelpFormatter(HelpFormatter):

    def _fill_text(self, text: str, width: int, indent: int):
        lines: list = text.splitlines()
        wrapped: list = [
            textwrap.fill(line, width,  initial_indent = indent, subsequent_indent = indent)
            for line in lines
        ]
        return "\n".join(wrapped)


def error(program: str, message: str) -> None:
    print(f"{program}: error: {message}")
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
