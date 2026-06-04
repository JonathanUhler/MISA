#!/usr/bin/env python3


from argparse import ArgumentParser, Namespace
import cmd
from dataclasses import dataclass, field
import os
import re
import readline
import subprocess
from subprocess import CompletedProcess
from typing import Any, Callable, Final
from helpers import format_subprocess_output, SmartHelpFormatter
from simulator import Simulator, Reg, Csr


HISTORY_FILE: Final = os.path.expanduser("~/.misa_history")


@dataclass
class CommandSpec:
    name: str
    callback: Callable
    usage: str
    short_desc: str
    long_desc: str = field(default_factory = str)
    aliases: list = field(default_factory = list)
    arg_types: list = field(default_factory = list)
    last_optional: bool = False
    last_default: Any = None


def parse_args(shell: "Shell",
               arg_str: str,
               types: list,
               last_optional: bool = False,
               last_default: Any = None) -> (bool, list | str):
    num_args: str = f"{len(types)}" if not last_optional else f"{len(types) - 1} or {len(types)}"
    if (arg_str is None):
        return (False, f"No arguments provided, expected {num_args}")

    args: list = arg_str.split()
    values: list = []
    if (len(args) > len(types)):
        return (False, f"Incorrect number of arguments, expected {num_args}, found {len(args)}")

    for i, arg in enumerate(args):
        try:
            values.append(types[i](shell, arg))
        except ValueError as e:
            return (False, f"Incorrect type for argument '{arg}' ({e})")
    if (len(args) < len(types) and last_optional):
        values.append(last_default)

    if (len(values) != len(types)):
        return (False, f"Incorrect number of arguments, expected {num_args}, found {len(args)}")
    return (True, values)


def register_commands(cls: type, commands: list) -> None:
    for spec in commands:
        def make_do(s: CommandSpec) -> Callable:
            def do_fn(self, arg_str: str):
                parsed, args = parse_args(
                    self,
                    arg_str,
                    s.arg_types,
                    last_optional = s.last_optional,
                    last_default = s.last_default
                )
                if (not parsed):
                    self.stdout.write(f"{s.usage}\n")
                    self.stdout.write(f"{args}\n")
                    return None
                return s.callback(self, args)
            do_fn.__doc__ = f"{s.usage} -- {s.short_desc}"
            do_fn.__name__ = f"do_{s.name}"
            return do_fn

        do_method: Callable = make_do(spec)
        setattr(cls, f"do_{spec.name}", do_method)
        for alias in spec.aliases:
            setattr(cls, f"do_{alias}", do_method)

        def make_help(s: CommandSpec) -> Callable:
            def help_fn(self):
                formatter: SmartHelpFormatter = SmartHelpFormatter("")
                formatted_long_desc: str = formatter._fill_text(s.long_desc,
                                                                formatter._width,
                                                                " " * formatter._current_indent)
                self.stdout.write(f"\033[1m{', '.join([s.name] + s.aliases)}\033[0m\n")
                self.stdout.write(f"{s.short_desc}\n")
                self.stdout.write(f"Usage: {s.usage}\n")
                if (len(formatted_long_desc) > 0):
                    self.stdout.write(f"{formatted_long_desc}\n")
            return help_fn

        help_method: Callable = make_help(spec)
        setattr(cls, f"help_{spec.name}", help_method)
        for alias in spec.aliases:
            setattr(cls, f"help_{alias}", help_method)


class Callbacks:

    @staticmethod
    def callback_load(shell: "Shell", args: list) -> None:
        try:
            shell.sim.load_mem(args[0], offset = args[1])
            shell.memory_file = args[0]
        except ValueError as e:
            shell.stdout.write(f"{e}\n")


    @staticmethod
    def callback_symbol_file(shell: "Shell", args: list) -> None:
        result: CompletedProcess = subprocess.run(["misa-nm", args[0]], capture_output = True)
        output: str = format_subprocess_output(result)
        new_symbols: dict = {
            sym: Types.integer(shell, f"0x{addr}")
            for addr, sym in re.findall(r"<- ([0-9a-fA-F]{4}) ([a-zA-Z0-9_]+)", output)
        }
        shell.symbols.update(new_symbols)
        shell.last_sym_file = args[0]
        shell.stdout.write(f"Imported {len(new_symbols)} new symbols\n")


    @staticmethod
    def callback_disassemble(shell: "Shell", args: list) -> None:
        if (args[0] is None):
            start, stop = max(0x0000, shell.sim.pc - 8), min(0xFFFF, shell.sim.pc + 16)
        else:
            start, stop = args[0]

        sym_file_options: list = \
            ["-g", f"{shell.last_sym_file}"] if shell.last_sym_file is not None else []
        result: CompletedProcess = subprocess.run(
            [
                "misa-objdump", "-D", shell.memory_file,
                "--start-address", f"{start}",
                "--stop-address", f"{stop}",
                *sym_file_options
            ],
            capture_output = True)

        output: str = format_subprocess_output(result)
        lines: list = output.splitlines()
        pc_str: str = "0x{:04X}".format(shell.sim.pc)
        lines_with_pc: list = [
            ("->" if line.startswith(pc_str) else "  ") + "  " + line for line in lines
        ]
        output_with_pc: str = "\n".join(lines_with_pc)

        shell.stdout.write(f"{output_with_pc}\n")


    @staticmethod
    def callback_reset(shell: "Shell", _: list) -> None:
        shell.sim.reset()
        shell.stdout.write(f"PC = {shell.sim.pc:#06x}\n")


    @staticmethod
    def callback_step(shell: "Shell", args: list) -> None:
        steps: int = 0
        for _ in range(args[0]):
            if (shell.sim.in_reset):
                shell.stdout.write("Simulator is halted.\n")
                break
            shell.sim.step()
            steps += 1
            if (shell.sim.pc in shell.breakpoints):
                shell.stdout.write(f"Breakpoint hit at {shell.sim.pc:#06x} after {steps} steps.\n")
                return
        shell.stdout.write(f"PC = {shell.sim.pc:#06x}\n")


    @staticmethod
    def callback_continue(shell: "Shell", _: list) -> None:
        steps: int = 0
        while (not shell.sim.in_reset):
            shell.sim.step()
            steps += 1
            if (shell.sim.pc in shell.breakpoints):
                shell.stdout.write(f"Breakpoint hit at {shell.sim.pc:#06x} after {steps} steps.\n")
                return
        shell.stdout.write(f"Simulator is halted (after {steps} steps).\n")
        shell.stdout.write(f"PC = {shell.sim.pc:#06x}\n")


    @staticmethod
    def callback_breakpoint(shell: "Shell", args: list) -> None:
        shell.breakpoints.add(args[0])
        shell.stdout.write(f"Breakpoint set at {args[0]:#06x}\n")


    @staticmethod
    def callback_delete(shell: "Shell", args: list) -> None:
        if (args[0] is None):
            shell.breakpoints.clear()
            shell.stdout.write("All breakpoints cleared.\n")
        else:
            shell.breakpoints.discard(args[0])
            shell.stdout.write(f"Breakpoint at {args[0]:#06x} removed.\n")


    @staticmethod
    def callback_info(shell: "Shell", args: list) -> None:
        if (args[0] in ("registers", "reg", "r")):
            Callbacks.callback_info_registers(shell)
        elif (args[0] in ("breakpoints", "break", "b")):
            Callbacks.callback_info_breakpoints(shell)
        else:
            shell.stdout.write(f"No info for '{args[0]}'\n")


    @staticmethod
    def callback_info_registers(shell: "Shell") -> None:
        for reg in Reg:
            val: int = shell.sim.reg[reg]
            shell.stdout.write(f"  {reg.name:<12} {val:#04x}  ({val:3d})")
            if ((reg + 1) % 2 == 0):
                shell.stdout.write("\n")

        shell.stdout.write("\n")

        for csr in Csr:
            val = shell.sim.csr[csr]
            shell.stdout.write(f"  {csr.name:<8} {val:#06x}  ({val:5d})\n")

        shell.stdout.write(f"  {'PC':<8} {shell.sim.pc:#06x}  ({shell.sim.pc:5d})\n")


    @staticmethod
    def callback_info_breakpoints(shell: "Shell") -> None:
        if (len(shell.breakpoints) == 0):
            shell.stdout.write("No breakpoints set.\n")
            return

        for addr in shell.breakpoints:
            shell.stdout.write(f"  {addr:#06x}")
            sym: str = next((k for k, v in shell.symbols.items() if v == addr), None)
            if (sym is not None):
                shell.stdout.write(f" ({sym})")
            shell.stdout.write("\n")


    @staticmethod
    def callback_examine(shell: "Shell", args: list) -> None:
        for i in range(0, args[1], 8):
            if (args[0] >= len(shell.sim.mem) or i >= len(shell.sim.mem)):
                break
            chunk: list = shell.sim.mem[args[0] + i : args[0] + i + 8]
            hex_part: str = " ".join(f"{b:02x}" for b in chunk)
            shell.stdout.write(f"  {args[0] + i:#06x}:  {hex_part}\n")


    @staticmethod
    def callback_quit(shell: "Shell", _: list) -> None:
        readline.write_history_file(HISTORY_FILE)
        shell.stdout.write("Goodbye.\n")
        return True


class Types:

    @staticmethod
    def integer(_: "Shell", string: str) -> int:
        return int(string, 0)


    @staticmethod
    def string(_: "Shell", string: str) -> str:
        return string


    @staticmethod
    def symbol(shell: "Shell", string: str) -> int:
        try:
            return Types.integer(shell, string)
        except ValueError:
            pass

        if (string not in shell.symbols):
            raise ValueError(f"Symbol '{string}' is not defined")
        return shell.symbols[string]


    @staticmethod
    def address_range(shell: "Shell", string: str) -> (int, int):
        parts: list = string.split(",")
        if (len(parts) != 2):
            raise ValueError(f"Address range '{string}' is not <start>,<stop> or <start>,<+count>")

        start: int = Types.symbol(shell, parts[0])
        if (parts[1].startswith("+")):
            count: int = Types.integer(shell, parts[1][1:])
            stop: int = start + count
        else:
            stop: int = Types.symbol(shell, parts[1])

        return (start, stop)


COMMANDS: Final = [
    CommandSpec(
        name          = "load",
        callback      = Callbacks.callback_load,
        usage         = "load <file> [offset]",
        short_desc    = "Load a binary image into memory and reset the simulator.",
        long_desc     = ("Given any file path, `load' will read the contents of the file and begin "
                         "loading bytes from the start of the file into the simulator memory at "
                         "the address given by [offset]. Loading ends when either the entire file "
                         "has been loaded, or the end of memory is reached. Bytes in the simulator "
                         "memory that are not touched by `load' have an undefined initial state."
                         "\n\n"
                         "After loading is completed, the simulator is reset in an equivalent "
                         "manner to executing the `reset' command. If the file fails to open, "
                         "the simulator state will not be affected."),
        arg_types     = [Types.string, Types.integer],
        last_optional = True,
        last_default  = 0x0000
    ),
    CommandSpec(
        name       = "symbol-file",
        aliases    = ["symbol", "sym"],
        callback   = Callbacks.callback_symbol_file,
        usage      = "symbol-file <file>",
        short_desc = "Load the symbol table from an object file.",
        long_desc  = ("Given a path to a MISA_LF object file, `symbol-file` will read the symbol "
                      "table from that object file and save the mapping of symbol names and "
                      "addresses for use by `breakpoint' and other commands."
                      "\n\n"
                      "If multiple loaded files contain the same symbol definition, only the "
                      "address of the last file loaded will be preserved. However, breakpoints "
                      "or other persistent references to the old symbol address will not be "
                      "retroactively changed by a new load."
                      "\n\n"
                      "Since object files use symbol addresses relative to the code segment in "
                      "a single translation unit, the recommended usage of `symbol-file' is with "
                      "the debug object file generated by `misa-ld -g <file>'. The flat binary "
                      "generated by the linker can be loaded with `load', the debug symbol file "
                      "can be loaded to easily reference symbols within the loaded binary."),
        arg_types  = [Types.string]
    ),
    CommandSpec(
        name          = "disassemble",
        aliases       = ["disass"],
        callback      = Callbacks.callback_disassemble,
        usage         = "disassemble [start,[stop|+count]]",
        short_desc    = "Disassemble a specific section of memory",
        long_desc     = ("Default is within +16/-8 bytes of the program counter value. The single "
                         "optional argument specifies a <start> byte address to begin disassembly "
                         "along with an optional <stop> address or offset <+count> from <start>. "
                         "If no stop/count is specified, a count of +16 will be used."
                         "\n\n"
                         "Disassembly requires that the program memory file used to start misa-sim "
                         "(or last loaded with `load') still exists on the disk. If the memory "
                         "file no longer exists, disassembly will not occur."
                         "\n\n"
                         "If a symbol file has previously been loaded with `symbol-file', and that "
                         "file still exists on disk, it will be used in the disassembly process to "
                         "fill in immediates and insert labels. Only the more recently-loaded "
                         "file will be used for this purpose."
                         "\n\n",
                         "Additionally, if a symbol file has been loaded, symbol names can be "
                         "used for the <start> and <stop> parameters. Symbol to address "
                         "translation will be performed using the most recently-loaded symbol "
                         "definitions."),
        arg_types     = [Types.address_range],
        last_optional = True,
        last_default  = None
    ),
    CommandSpec(
        name       = "reset",
        callback   = Callbacks.callback_reset,
        usage      = "reset",
        short_desc = "Jump to the reset vector.",
        long_desc  = ("Resetting the processor performs all steps required by the MISA instruction "
                      "set architecture. Upon reset, the program counter will be set to the "
                      "address of the reset vector. The processor will immediately be ready for "
                      "instruction execution via `step' or `continue'.")
    ),
    CommandSpec(
        name          = "step",
        aliases       = ["si", "s"],
        callback      = Callbacks.callback_step,
        usage         = "step [n]",
        short_desc    = "Single-step by a certain number of instructions.",
        long_desc     = ("If the processor is not halted, at most [n] instructions will be "
                         "simulated. Stepping of many instructions will end early if the "
                         "processor halts for any reason. Faults and system calls will not stop "
                         "the instruction stepping. By default, [n] is 1."),
        arg_types     = [Types.integer],
        last_optional = True,
        last_default  = 1
    ),
    CommandSpec(
        name       = "continue",
        aliases    = ["cont", "c"],
        callback   = Callbacks.callback_continue,
        usage      = "continue",
        short_desc = "Run until a breakpoint or halt."
    ),
    CommandSpec(
        name       = "breakpoint",
        aliases    = ["break", "br", "b"],
        callback   = Callbacks.callback_breakpoint,
        usage      = "breakpoint <addr|sym>",
        short_desc = "Set a breakpoint at an address or imported symbol.",
        long_desc  = ("Breakpoints may be set either by specifying an absolute address in memory "
                      "or by specifying the name of a symbol imported via `symbol-file'. Symbol "
                      "breakpoints will be translated to addresses immediately and thus be "
                      "unaffected by subsequent `symbol-file' commands."
                      "\n\n"
                      "When the program counter is incremented to an address with a breakpoint, "
                      "the `step' and `continue' commands will stop advancing the simulation and "
                      "report the hit breakpoint. Breakpoints must be placed at the lower byte "
                      "of instructions to hit correctly."),
        arg_types  = [Types.symbol]
    ),
    CommandSpec(
        name          = "delete",
        aliases       = ["del", "d"],
        callback      = Callbacks.callback_delete,
        usage         = "delete [addr|sym]",
        short_desc    = "Remove a breakpoint.",
        long_desc     = ("Breakpoints may be deleted either by specifying an absolute address in "
                         "memory of an existing breakpoint or by specifying the name of a symbol "
                         "imported via `symbol-file'. Specifying a symbol will cause a lookup "
                         "of that symbol's address (if imported) and then an attempt to delete "
                         "a breakpoint with that address."
                         "\n\n"
                         "All breakpoints can be deleted by specifying `delete' with no "
                         "arguments. If the specified breakpoint address does not exist, nothing "
                         "will happen to remaining breakpoints."),
        arg_types     = [Types.symbol],
        last_optional = True
    ),
    CommandSpec(
        name       = "info",
        aliases    = ["i"],
        callback   = Callbacks.callback_info,
        usage      = "info <topic>",
        short_desc = "Display simulator state.",
        long_desc  = ("\033[1minfo breakpoints\033[0m -- Display the list of active breakpoints."
                      "\n"
                      "\033[1minfo registers\033[0m -- Display the contents of all registers."),
        arg_types  = [Types.string]
    ),
    CommandSpec(
        name          = "examine",
        aliases       = ["exam", "x"],
        callback      = Callbacks.callback_examine,
        usage         = "examine <addr> [count]",
        short_desc    = "Dump memory as hex bytes.",
        arg_types     = [Types.integer, Types.integer],
        last_optional = True,
        last_default  = 16
    ),
    CommandSpec(
        name       = "quit",
        aliases    = ["q"],
        callback   = Callbacks.callback_quit,
        usage      = "quit",
        short_desc = "Exit the simulator"
    )
]


class Shell(cmd.Cmd):

    intro: str = "MISA Functional Simulator. Type 'help' for commands, 'quit' to exit."
    prompt: str = "(misa) "


    def print_topics(self, header, cmds, _unused2, _unused3):
        if (not cmds):
            return

        self.stdout.write(f"{header}\n")
        self.stdout.write("=" * len(header) + "\n")
        for command in COMMANDS:
            self.stdout.write(f"  \033[1m{command.name}\033[0m -- {command.short_desc}\n")
        self.stdout.write("\n")


    def __init__(self, binary_file: str | None = None):
        super().__init__()
        self.sim = Simulator()
        self.symbols: dict = {}
        self.breakpoints: set = set()
        self.memory_file = binary_file
        self.last_sym_file = None

        if (self.memory_file is not None):
            self.sim.load_mem(self.memory_file)

        try:
            readline.read_history_file(HISTORY_FILE)
        except FileNotFoundError:
            pass
        readline.set_history_length(1000)


def main() -> None:
    parser: ArgumentParser = ArgumentParser(description = "MISA functional simulator")
    parser.add_argument("binary", nargs = "?", help = "Binary image to load at startup")
    args: Namespace = parser.parse_args()

    shell: Shell = Shell(args.binary)
    try:
        shell.cmdloop()
    except KeyboardInterrupt:
        getattr(shell, "do_quit")("")


if (__name__ == "__main__"):
    register_commands(Shell, COMMANDS)
    main()
