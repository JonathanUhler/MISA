# MISA

Minimal Instruction Set Architecture (MISA) is a limited instruction set and computer architecture
designed for educational purposes. The ultimate design goal is to apply a large set of the knowledge
from the average computer science undergraduate degree into one project.

## Project Components

The project includes the following software applications:

- `misa-as`: An assembler for the MISA instruction set that produces object files
- `misa-ld`: A linker to produce flat binaries from one or more object files
- `misa-nm`: A program to list symbol names imported and exported by object files
- `misa-sim`: A faithful simulator for a single-cycle CPU implementation of the MISA architecture

## Documentation

The `docs` directory includes a full specification of the project, split into a functional block
diagram of the computer implemented by `misa-sim`, and a architecture book describing the ISA,
build toolchain, and simulator.

## Building

Building the MISA toolchain and simulator requires:

- Stack 3.3.0 or later
- A recent Haskell compiler
- Cmake 3.22.0 or later
- A recent GNU-based C++ compiler

Typing `make` in the root directory of the cloned repository will build all components of the
project. Other make targets include:

- `build-toolchain`: To build just the toolchain applications (everything but the simulator)
- `build-sim`: To build just the simulator
- `clean`: To remove build artifacts
