# MISA

Minimal Instruction Set Architecture (MISA) is a limited instruction set and computer architecture
designed for educational purposes. The ultimate design goal is to apply a large set of the knowledge
from the average computer science undergraduate degree into one project.

## Project Components

The project includes the following software applications:

- `misa-ar`: An archiver to bundle several object files together
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
- Python 3.10 or later

Typing `make` in the root directory of the cloned repository will build all components of the
project.

## Installing and Usage

The project components can be installed with `make install`. By default, the installation will be
made in the local `install` directory. A custom path can be set by specifying the Make variable
`INSTALL_DIR`.

The install directory will be populated with the following tools:

- `misa-ar`, `misa-as`, and `mias-ld`: CLI utilities written in Python to make the usage of the
  Haskell binaries easier.
- `misa-ar-exe`, `misa-as-exe`, `misa-ld-exe`, and `misa-nm-exe`: binaries built from the Haskell
  source that provide a very minimal command line interface for use by the Python CLI wrappers.
