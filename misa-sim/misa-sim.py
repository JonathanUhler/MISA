#!/usr/bin/env python3


from argparse import ArgumentParser, Namespace
import cmd
import inspect
import os
import readline
from simulator import Simulator, Reg, Csr


HISTORY_FILE = os.path.expanduser("~/.misa_history")


class Shell(cmd.Cmd):

    intro: str = "MISA Functional Simulator. Type 'help' for commands, 'quit' to exit."
    prompt: str = "(misa) "


    def print_topics(self, header, cmds, cmdlen, maxcol):
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


    def __init__(self, binary: str | None = None):
        super().__init__()
        self.sim = Simulator()
        self._last_cmd: str = ""
        self._breakpoints: set[int] = set()
        self._running: bool = True

        if (binary):
            self._load(binary)

        try:
            readline.read_history_file(HISTORY_FILE)
        except FileNotFoundError:
            pass
        readline.set_history_length(1000)


    def _load(self, path: str) -> None:
        try:
            with open(path, "rb") as f:
                data: bytes = f.read()
            for i, byte in enumerate(data):
                self.sim.mem[i] = byte
            self.sim.reset()
            print(f"Loaded {len(data)} bytes from '{path}', PC = {self.sim.pc:#06x}")
        except OSError as e:
            print(f"Error loading file: {e}")


    def _resolve_addr(self, arg: str) -> int | None:
        try:
            return int(arg, 0)
        except ValueError:
            print(f"Invalid address: {arg!r}")
            return None


    def default(self, line: str) -> None:
        if (line == "" and self._last_cmd is not None):
            self.onecmd(self._last_cmd)
        else:
            print(f"Unknown command: {line!r}. Try 'help'.")


    def postcmd(self, stop: bool, line: str) -> bool:
        if (line.strip() != ""):
            self._last_cmd = line.strip()
        return stop


    def do_load(self, arg: str) -> None:
        """load <file> -- Load a binary image and reset the simulator."""
        if (arg is None):
            print("Usage: load <file>")
            return
        self._load(arg.strip())


    def do_reset(self, _: str) -> None:
        """reset -- Toggle the reset pin of the simulator."""
        self.sim.reset()
        print(f"Reset. PC = {self.sim.pc:#06x}")


    def do_step(self, arg: str) -> None:
        """step [n] -- Execute n instructions (default 1)."""
        n: int = 1
        if (arg is not None):
            try:
                n = int(arg, 0)
            except ValueError:
                print("Usage: step [count]")
                return

        for _ in range(n):
            if (self.sim.in_reset):
                print("Simulator is halted.")
                break
            self.sim.step()
            print(f"PC = {self.sim.pc:#06x}")

    do_s = do_step


    def do_continue(self, _: str) -> None:
        """continue -- Run until a breakpoint or halt."""
        if (self.sim.in_reset):
            print("Simulator is halted.")
            return

        steps: int = 0
        while (not self.sim.in_reset):
            self.sim.step()
            steps += 1
            if (self.sim.pc in self._breakpoints):
                print(f"Breakpoint hit at {self.sim.pc:#06x} after {steps} steps.")
                return
        print(f"Halted after {steps} steps. PC = {self.sim.pc:#06x}")

    do_c = do_continue


    def do_break(self, arg: str) -> None:
        """break <addr> -- Set a breakpoint at address."""
        addr: int = self._resolve_addr(arg)
        if (addr is None):
            return
        self._breakpoints.add(addr)
        print(f"Breakpoint set at {addr:#06x}")

    do_b = do_break


    def do_delete(self, arg: str) -> None:
        """delete <addr> -- Remove a breakpoint. 'delete all' clears all."""
        if (arg.strip() == "all"):
            self._breakpoints.clear()
            print("All breakpoints cleared.")
            return

        addr: int = self._resolve_addr(arg)
        if addr is None:
            return

        self._breakpoints.discard(addr)
        print(f"Breakpoint at {addr:#06x} removed.")

    do_d = do_delete


    def do_info(self, arg: str) -> None:
        """info registers | breakpoints -- Display simulator state."""
        sub: str = arg.strip().lower()
        if (sub in ("registers", "reg", "r")):
            self._print_registers()
        elif (sub in ("breakpoints", "break", "b")):
            if (self._breakpoints is not None):
                for addr in sorted(self._breakpoints):
                    print(f"  {addr:#06x}")
            else:
                print("No breakpoints set.")
        else:
            print("Usage: info registers | info breakpoints")

    do_i = do_info


    def _print_registers(self) -> None:
        for reg in Reg:
            val = self.sim.reg[reg]
            print(f"  {reg.name:<12} {val:#04x}  ({val:3d})", end="")
            if ((reg + 1) % 2 == 0):
                print()

        print()

        for csr in Csr:
            val = self.sim.csr[csr]
            print(f"  {csr.name:<8} {val:#06x}  ({val:5d})")

        print(f"  {'PC':<8} {self.sim.pc:#06x}  ({self.sim.pc:5d})")


    def do_examine(self, arg: str) -> None:
        """examine <addr> [count] -- Dump memory as hex bytes."""
        parts: list = arg.split()
        if (not parts):
            print("Usage: examine <addr> [count]")
            return

        addr: int = self._resolve_addr(parts[0])
        if (addr is None):
            return

        count: int = int(parts[1], 0) if len(parts) > 1 else 16
        for i in range(0, count, 8):
            chunk: list = self.sim.mem[addr + i : addr + i + 8]
            hex_part: str = " ".join(f"{b:02x}" for b in chunk)
            print(f"  {addr + i:#06x}:  {hex_part}")

    do_x = do_examine


    def do_quit(self, _: str) -> bool:
        """quit  --  Exit the simulator."""
        readline.write_history_file(HISTORY_FILE)
        print("Goodbye.")
        return True

    do_q = do_quit


def main() -> None:
    parser: ArgumentParser = ArgumentParser(description = "MISA functional simulator")
    parser.add_argument("binary", nargs = "?", help = "Binary image to load at startup")
    args: Namespace = parser.parse_args()

    shell: Shell = Shell(binary = args.binary)
    try:
        shell.cmdloop()
    except KeyboardInterrupt:
        shell.do_quit("")


if (__name__ == "__main__"):
    main()
