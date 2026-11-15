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
- `misa-objdump`: A program to disassemble object files and flat binaries into assembly
- `misa-pr`: A preprocessor to add additional functionality to the assembly pipeline
- `misa-spec`: A behavioral model of the MISA instruction set in Lean4, intended to be used as
  a reference 'correct'/'golden' implementation of the architecture.

## Documentation

The `docs` directory includes a full specification of the project, including the ISA manual and
notes about the workings/development of the build toolchain and simulators.

## Building

Building the MISA toolchain requires:

- Stack 3.3.0 or later
- A recent Haskell compiler
- Lake 5.0.0 or later
- A recent Lean compiler
- Python 3.11 or later

Typing `make` in the root directory of the cloned repository will build all components of the
project.

## Installing and Usage

The project components can be installed with `make install`. By default, the installation will be
made in the local `install` directory. A custom path can be set by specifying the Make variable
`INSTALL_DIR`.

The install directory will be populated with the following tools:

- `misa-ar`, `misa-as`, `misa-ld`, `misa-nm`, `misa-objdump`, `misa-pr`, and `misa-spec`: CLI
  utilities written in Python to make the usage of the Haskell and Lean binaries easier.
- `misa-ar-exe`, `misa-as-exe`, `misa-ld-exe`, `misa-nm-exe`, `misa-objdump-exe`, `misa-pr-exe` and
  `misa-spec-exe`: binaries built from the Haskell and Lean source that provide a very minimal
  command line interface for use by the Python CLI wrappers.
