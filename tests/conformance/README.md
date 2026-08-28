# Conformance Tests

This directory holds the conformance suite for the MISA reference simulator. Each test assembles and
links a small assembly program with the toolchain, runs it in the simulator, and checks the final
processor state. This suite is the primary guard against the simulator mismatching with the ISA
specification.

## Prerequisites

The suite drives the installed toolchain, so the project must be built first. Run `make` from the
repository root.

```
make
```

Building populates the `install` directory with the `misa-as` wrapper and the Haskell binaries. The
suite adds that directory to the PATH on its own. It also imports the simulator directly from
`misa-sim/simulator.py`.

## Running

Run the suite from the repository root.

```
pytest tests/conformance
```

Each test builds its program from a fixture in the `programs` directory, runs it to a halt, and
asserts on registers, flags, `CAUSE`, or memory. A program that does not halt within the step limit
fails the test.

## Layout

The suite has four parts.

- `harness.py` builds and runs programs and exposes the simulator state.
- `conftest.py` skips the suite with a clear message when the toolchain is not built.
- `programs` holds the assembly fixtures.
- `test_baseline.py` covers arithmetic, memory access, and a subroutine call.
