#!/usr/bin/env python3


from argparse import ArgumentParser, Namespace
import cmd
import inspect
import os
import readline
from typing import Any
from simulator import Simulator, Reg, Csr


HISTORY_FILE: str = os.path.expanduser("~/.misa_history")


class Shell(cmd.Cmd):

    intro: str = "MISA Functional Simulator. Type 'help' for commands, 'quit' to exit."
    prompt: str = "(misa) "


    def print_topics(self, header, cmds, cmdlen, maxcol):  # MARK: clean this up
        if (not cmds):
            return

        self.stdout.write(f"{header}\n")
        self.stdout.write("=" * len(header) + "\n")

        for command in cmds:
            method = getattr(self, f'do_{command}', None)
            doc = inspect.getdoc(method) if method else None
            summary = doc.split('\n')[0] if doc else "No description available."
            self.stdout.write(f"  {command:<12} - {summary}\n")

        self.stdout.write("\n")


    def __init__(self, binary_file: str | None = None):
        super().__init__()
        self.sim = Simulator()
        self._breakpoints: set = set()

        if (binary_file is not None):
            self._load_memory(binary_file, 0x0000)

        try:
            readline.read_history_file(HISTORY_FILE)
        except FileNotFoundError:
            pass
        readline.set_history_length(1000)


    def _load_memory(self, binary_file: str, offset: int) -> None:
        try:
            with open(binary_file, "rb") as f:
                binary: bytes = f.read()
            if (len(binary) - offset > len(self.sim.mem)):
                self.stdout.write(f"Binary file ({len(binary)} bytes) is too large\n")

            for i, byte in enumerate(binary):
                self.sim.mem[i + offset] = byte
            self.sim.reset()
            self.stdout.write(f"Loaded {len(binary)} bytes, PC = {self.sim.pc:#06x}\n")
        except OSError as e:
            self.stdout.write(f"Error loading file: {e}\n")


    def _parse_args(self,
                    arg_str: str,
                    types: list,
                    last_optional: bool = False,
                    last_default: Any = None) -> list:
        if (arg_str is None):
            self.stdout.write("Incorrect number of arguments\n")
            return None

        args: list = arg_str.split()
        values: list = []

        for i, arg in enumerate(args):
            try:
                values.append(types[i](arg))
            except ValueError:
                self.stdout.write(f"Incorrect type for argument '{arg}'\n")
                return None

        if (len(types) == len(args)):
            return values
        elif (len(types) == len(args) + 1 and last_optional):
            values.append(last_default)
            return values
        else:
            self.stdout.write("Incorrect number of arguments\n")
            return None


    def do_load(self, arg_str: str) -> None:
        """load <file> [offset] -- Load a binary image and reset the simulator."""
        args: list = self._parse_args(arg_str, [str, lambda x: int(x, 0)],
                                      last_optional = True, last_default = 0x0000)
        if (args is None):
            return

        self._load_memory(args[0], args[1])


    def do_reset(self, _: str) -> None:
        """reset -- Toggle the reset pin of the simulator."""
        self.sim.reset()
        self.stdout.write(f"Reset. PC = {self.sim.pc:#06x}\n")


    def do_step(self, arg_str: str) -> None:
        """step [n] -- Execute n instructions (default 1)."""
        args: list = self._parse_args(arg_str, [lambda x: int(x, 0)],
                                      last_optional = True, last_default = 1)
        if (args is None):
            return

        for _ in range(args[0]):
            if (self.sim.in_reset):
                self.stdout.write("Simulator is halted.\n")
                break
            self.sim.step()
            self.stdout.write(f"PC = {self.sim.pc:#06x}\n")

    do_s = do_step


    def do_continue(self, _: str) -> None:
        """continue -- Run until a breakpoint or halt."""
        if (self.sim.in_reset):
            self.stdout.write("Simulator is halted.\n")
            return

        steps: int = 0
        while (not self.sim.in_reset):
            self.sim.step()
            steps += 1
            if (self.sim.pc in self._breakpoints):
                self.stdout.write(f"Breakpoint hit at {self.sim.pc:#06x} after {steps} steps.\n")
                return
        self.stdout.write(f"Halted after {steps} steps. PC = {self.sim.pc:#06x}\n")

    do_c = do_continue


    def do_break(self, arg_str: str) -> None:
        """break <addr> -- Set a breakpoint at address."""
        args: list = self._parse_args(arg_str, [lambda x: int(x, 0)])
        if (args is None):
            return

        self._breakpoints.add(args[0])
        self.stdout.write(f"Breakpoint set at {args[0]:#06x}\n")

    do_b = do_break


    def do_delete(self, arg_str: str) -> None:
        """delete <addr> -- Remove a breakpoint. 'delete all' clears all."""
        args: list = self._parse_args(arg_str, [lambda x: int(x, 0)],
                                      last_optional = True, last_default = "all")
        if (args is None):
            return

        if (args[0] == "all"):
            self._breakpoints.clear()
            self.stdout.write("All breakpoints cleared.\n")
        else:
            self._breakpoints.discard(args[0])
            self.stdout.write(f"Breakpoint at {args[0]:#06x} removed.\n")

    do_d = do_delete


    def do_info(self, arg_str: str) -> None:
        """info registers | breakpoints -- Display simulator state."""
        args: list = self._parse_args(arg_str, [str])

        if (args[0] in ("registers", "reg", "r")):
            self._print_registers()
        elif (args[1] in ("breakpoints", "break", "b")):
            self._print_breakpoints()
        else:
            self.stdout.write(f"No info for '{args[0]}'\n")

    do_i = do_info


    def _print_registers(self) -> None:
        for reg in Reg:
            val: int = self.sim.reg[reg]
            self.stdout.write(f"  {reg.name:<12} {val:#04x}  ({val:3d})")
            if ((reg + 1) % 2 == 0):
                self.stdout.write("\n")

        self.stdout.write("\n")

        for csr in Csr:
            val = self.sim.csr[csr]
            self.stdout.write(f"  {csr.name:<8} {val:#06x}  ({val:5d})\n")

        self.stdout.write(f"  {'PC':<8} {self.sim.pc:#06x}  ({self.sim.pc:5d})\n")


    def _print_breakpoints(self) -> None:
        if (len(self._breakpoints) == 0):
            self.stdout.write("No breakpoints set.\n")
            return

        for addr in self._breakpoints:
            self.stdout.write(f"  {addr:#06x}\n")


    def do_examine(self, arg_str: str) -> None:
        """examine <addr> [count] -- Dump memory as hex bytes."""
        args: list = self._parse_args(arg_str, [lambda x: int(x, 0), int],
                                      last_optional = True, last_default = 16)

        for i in range(0, args[1], 8):
            chunk: list = self.sim.mem[args[0] + i : args[0] + i + 8]
            hex_part: str = " ".join(f"{b:02x}" for b in chunk)
            self.stdout.write(f"  {args[0] + i:#06x}:  {hex_part}\n")

    do_x = do_examine


    def do_quit(self, _: str) -> bool:
        """quit  --  Exit the simulator."""
        readline.write_history_file(HISTORY_FILE)
        self.stdout.write("Goodbye.\n")
        return True

    do_q = do_quit


def main() -> None:
    parser: ArgumentParser = ArgumentParser(description = "MISA functional simulator")
    parser.add_argument("binary", nargs = "?", help = "Binary image to load at startup")
    args: Namespace = parser.parse_args()

    shell: Shell = Shell(args.binary)
    try:
        shell.cmdloop()
    except KeyboardInterrupt:
        shell.do_quit("")


if (__name__ == "__main__"):
    main()
