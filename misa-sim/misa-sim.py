#!/usr/bin/env python3


from argparse import ArgumentParser, Namespace
import cmd
from dataclasses import dataclass, field
import os
import readline
from typing import Any, Callable, Final
from simulator import Simulator, Reg, Csr


HISTORY_FILE: Final = os.path.expanduser("~/.misa_history")


@dataclass
class CommandSpec:
    name: str
    callback: Callable
    usage: str
    short_desc: str
    long_desc: str
    aliases: list = field(default_factory = list)
    arg_types: list = field(default_factory = list)
    last_optional: bool = False
    last_default: Any = None


def parse_args(arg_str: str,
               types: list,
               last_optional: bool = False,
               last_default: Any = None) -> (bool, list | str):
    num_args: str = f"{len(types)}" if not last_optional else f"{len(types) - 1} or {len(types)}"
    if (arg_str is None):
        return (False, f"No arguments provided, expected {num_args}")

    args: list = arg_str.split()
    values: list = []

    for i, arg in enumerate(args):
        try:
            values.append(types[i](arg))
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
                self.stdout.write(f"\033[1m{', '.join([s.name] + s.aliases)}\033[0m\n")
                self.stdout.write(f"{s.short_desc}\n")
                self.stdout.write(f"{s.usage}\n")
                self.stdout.write(f"{s.long_desc}\n")
            return help_fn

        help_method: Callable = make_help(spec)
        setattr(cls, f"help_{spec.name}", help_method)
        for alias in spec.aliases:
            setattr(cls, f"help_{alias}", help_method)


class Callbacks:

    @staticmethod
    def callback_load(shell: "Shell", args: list) -> None:
        shell.sim.load_mem(args[0], offset = args[1])


    @staticmethod
    def callback_reset(shell: "Shell", _: list) -> None:
        shell.sim.reset()
        shell.stdout.write(f"Reset. PC = {shell.sim.pc:#06x}\n")


    @staticmethod
    def callback_step(shell: "Shell", args: list) -> None:
        for _ in range(args[0]):
            if (shell.sim.in_reset):
                shell.stdout.write("Simulator is halted.\n")
                break
            shell.sim.step()
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
        shell.stdout.write(f"Halted after {steps} steps. PC = {shell.sim.pc:#06x}\n")


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
            shell.stdout.write(f"  {addr:#06x}\n")


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


COMMANDS: Final = [
    CommandSpec(
        name          = "load",
        callback      = Callbacks.callback_load,
        usage         = "load <file> [offset]",
        short_desc    = "Load a binary image into memory and reset the simulator.",
        long_desc     = (""),
        arg_types     = [str, lambda x: int(x, 0)],
        last_optional = True,
        last_default  = 0x0000
    ),
    CommandSpec(
        name       = "reset",
        callback   = Callbacks.callback_reset,
        usage      = "reset",
        short_desc = "Jump to the reset vector.",
        long_desc  = ("")
    ),
    CommandSpec(
        name          = "step",
        aliases       = ["si", "s"],
        callback      = Callbacks.callback_step,
        usage         = "step [n]",
        short_desc    = "Single-step by a certain number of instructions.",
        long_desc     = (""),
        arg_types     = [lambda x: int(x, 0)],
        last_optional = True,
        last_default  = 1
    ),
    CommandSpec(
        name       = "continue",
        aliases    = ["cont", "c"],
        callback   = Callbacks.callback_continue,
        usage      = "continue",
        short_desc = "Run until a breakpoint or halt.",
        long_desc  = ("")
    ),
    CommandSpec(
        name       = "breakpoint",
        aliases    = ["break", "br", "b"],
        callback   = Callbacks.callback_breakpoint,
        usage      = "breakpoint <addr|sym>",
        short_desc = "Set a breakpoint at an address or imported symbol.",
        long_desc  = (""),
        arg_types  = [lambda x: int(x, 0)]  # MARK: add symbol support
    ),
    CommandSpec(
        name          = "delete",
        aliases       = ["del", "d"],
        callback      = Callbacks.callback_delete,
        usage         = "delete [addr|sym]",
        short_desc    = "Remove a breakpoint.",
        long_desc     = (""),
        arg_types     = [lambda x: int(x, 0)],  # MARK: add symbol support
        last_optional = True
    ),
    CommandSpec(
        name       = "info",
        aliases    = ["i"],
        callback   = Callbacks.callback_info,
        usage      = "info <topic>",
        short_desc = "Display simulator state.",
        long_desc  = (""),
        arg_types  = [str]
    ),
    CommandSpec(
        name          = "examine",
        aliases       = ["exam", "x"],
        callback      = Callbacks.callback_examine,
        usage         = "examine <addr> [count]",
        short_desc    = "Dump memory as hex bytes.",
        long_desc     = (""),
        arg_types     = [lambda x: int(x, 0), lambda x: int(x, 0)],
        last_optional = True,
        last_default  = 16
    ),
    CommandSpec(
        name       = "quit",
        aliases    = ["q"],
        callback   = Callbacks.callback_quit,
        usage      = "quit",
        short_desc = "Exit the simulator",
        long_desc  = ("")
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
        self.breakpoints: set = set()

        if (binary_file is not None):
            self.sim.load_mem(binary_file)

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
