#!/usr/bin/python3


from argparse import ArgumentParser, Namespace
from pathlib import Path
import re
from re import Match
import subprocess
from subprocess import CompletedProcess
from helpers import (SmartHelpFormatter,
                     error,
                     format_subprocess_output)


program_name: str = "misa-objdump"
version_text: str = "0.1.0.0"
help_text: str = (
    "misa-objdump"
)


def infer_binary_format(bin_path: Path) -> str:
    try:
        with open(bin_path, "rb") as f:
            binary: bytes = f.read()
            if (binary.startswith("MISA_LF ".encode())):
                return "misa_lf"
            return "binary"
    except OSError:
        return None


def disassemble(bin_path: Path, format: str, decode_all: bool, sym_file: Path) -> (bool, str):
    if (format == "misa_lf"):
        other_args: list = [str(decode_all)]
    elif (format == "binary"):
        if (not decode_all):
            return (False, "no code section to decode in a 'binary' format file, specify -D")
        other_args: list = [sym_file] if sym_file is not None else []
    else:
        return (False, f"unrecognized file format '{format}'")

    result: CompletedProcess = \
        subprocess.run(["misa-objdump-exe", format, bin_path, *other_args], capture_output = True)
    disassembled: bool = result.returncode == 0
    return disassembled, format_subprocess_output(result)


def split_sections(lines: list) -> list:
    sections: list = []
    current: list = []

    for line in lines:
        if (re.match(r"^\s*\.section\b", line) and current):
            sections.append(current)
            current = [line]
        else:
            current.append(line)
    if (current):
        sections.append(current)

    return sections


def split_blocks(lines: list) -> list:
    # Each block: (address | None,  [lines belonging to this block])
    #
    # A block carries all addressless lines that preceded the addressed line (or, for the very
    # last block, any trailing addressless lines).
    blocks: list = []
    current: list = []

    for line in lines:
        addr_match: Match = re.match(r"^(0x[0-9A-Fa-f]{4})\s", line)
        current.append(line)
        if (addr_match):
            blocks.append((int(addr_match.group(1), 16), current))
            current = []
    if (current):
        blocks.append((None, current))

    return blocks


def include_line(line_addr: int, start: int, stop: int) -> bool:
    start_ok: bool = (start is None) or (line_addr >= start)
    stop_ok: bool = (stop is None) or (line_addr <= stop)
    return start_ok and stop_ok


def filter_lines(disassembly: str, start: int, stop: int) -> str:
    lines: list = disassembly.splitlines()
    sections: list = split_sections(lines)

    filtered: list = []
    for sec_lines in sections:
        blocks: list = split_blocks(sec_lines)
        sec_addrs: list = [a for a, _ in blocks if a is not None]

        if (not sec_addrs and start is None):
            for _, blk in blocks:
                filtered.extend(blk)
            continue

        if (start is not None):
            floors: int = [a for a in sec_addrs if a <= start]
            start = max(floors) if floors else start

        for i, (addr, block) in enumerate(blocks):
            if (addr is not None and include_line(addr, start, stop)):
                filtered.extend(block)

    return '\n'.join(filtered)


def main() -> None:
    """
    Command line entry point for misa-objdump.
    """

    parser: ArgumentParser = ArgumentParser(
        prog = program_name,
        formatter_class = SmartHelpFormatter,
        description = help_text
    )
    parser.add_argument("-d", "--disassemble", action = "store_true",
                        help = "disassemble sections of the input file expected to contain code")
    parser.add_argument("-D", "--disassemble-all", action = "store_true",
                        help = "disassemble all sections or content of the input file")
    parser.add_argument("-f", "--format", metavar = "<format>", choices = {"misa_lf", "binary"},
                        help = "treat the input file format as <format>, infer by default")
    parser.add_argument("-g", "--debug", type = Path, metavar = "<file>",
                        help = "load the symbols and relocations from the debug object file <file>")
    parser.add_argument("--start-address", type = lambda x: int(x, 0), metavar = "<address>",
                        help = "start displaying data at or after <address>")
    parser.add_argument("--stop-address", type = lambda x: int(x, 0), metavar = "<address>",
                        help = "stop displaying data at or before <address>")
    parser.add_argument("-V", "--version", action = "version", version = f"%(prog)s {version_text}")
    parser.add_argument("binfile", type = Path, metavar = "<binfile>",
                        help = "display information from the binary file <binfile>")

    args: Namespace = parser.parse_args()

    if (not args.format):
        args.format = infer_binary_format(args.binfile)
        if (args.format is None):
            error(program_name, f"{args.binfile}: binary format not recognized")

    disassembled, output = False, None
    if (args.disassemble_all):
        disassembled, output = \
            disassemble(args.binfile, args.format, decode_all = True, sym_file = args.debug)
    elif (args.disassemble):
        disassembled, output = \
            disassemble(args.binfile, args.format, decode_all = False, sym_file = args.debug)
    else:
        error(program_name, f"no actions to take, specify one of: -d, -D")

    if (not disassembled):
        error(program_name, output)
    else:
        output = filter_lines(output, start = args.start_address, stop = args.stop_address)
        print(output)


if (__name__ == "__main__"):
    main()
