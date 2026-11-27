# `misa-hw` Testbench

A differential testbench for the Amaranth implementation of MISA. The golden model used to compare
with `misa-hw` is `misa-spec`, which may be assumed to agree with the ISA manual.

The testbench is organized to cover two major parameters:

1. Speed/exhaustiveness of tests
2. Target model for tests (Amaranth source, elaborated Verilog, synthesized gates)

This is done with different test "campaigns," which each cover different target models with
different kinds and quantities of tests. The test methodology is described more in the next
sections.

## Running

Tests are run with the pytest library. The following command line options are available to change
test behavior:

| Option | Purpose |
|----------------|----------------------------------------------------------------------------------------|
| `--target` | Which target model to run tests against.  |
| `--compare` | Compare every state ("lockstep") or only the final state after a test program is run. |
| `--latency` | The memory access latency pattern to use when `MisaCore` performs memory transactions. |
| `--exhaustive` | Whether to run the full number of tests for every campaign, or only a quicker subset.  |
| `--seeds` | The number of seeds to use per test campaign.  |

The following pytest markers are also available:

| Marker | Purpose |
|---------------|---------------------------------------------------------------|
| `subset` | Part of the Level B and Level B test subset.  |
| `exhaustive` | Long tests that are only run when `--exhaustive` is provided. |
| `random` | Tests with randomized inputs.  |
| `equivalence` | Formal verification and netlist equivalence tests.  |
| `slow` | Long-running tests.  |

## Campaigns

| Module | Purpose |
|-------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `test_harness.py` | Tests the testbench itself (state file serdes, state record replay, the memory BFM, instruction boundary detection, etc).  |
| `test_units.py` | `AluUnit`, `CmpUnit`, `DecoderUnit`, `GpRegFile`, `CsrRegFile` and `MpuUnit` individual tests. `--exhaustive` covers every ALU operand pair and every instruction word against every `EXTNS` value. |
| `test_sweep.py` | Executes one instruction from a known state and compares the whole architectural state, for every one of the 65536 instruction words. |
| `test_directed.py` | A set of directed tests that verify conformance for behavior in the ISA manual (decode legality, flags, addressing, fault precedence, vector reads, special registers, and each extension). |
| `test_programs.py` | Runs every program in `programs/bringup` against `misa-spec`, and checks that each one halts with the correct finish code it reports on success. |
| `test_pairs.py` | Executes two-instruction sequences from a known state. |
| `test_random.py` | Fills memory with random bytes and a valid vector table. |
| `test_weighted.py` | Generates structured random programs from a set of profiles, each with a valid prologue, body, and epilogue. |
| `test_interrupts.py` | Tests the interrupt pin edge detection and interrupt handler. |

### `test_units` Reference Model

`test_units.py` is the only campaign that does not use `misa-spec` as a golden model, because the
units have no architectural state in common with the `misa-spec` state files. Its uses a separate
reference model written from the ISA manual.

### `test_sweep` and `test_pairs` Seeds

Executing one instruction from the canonical reset state is unlikely to catch many bugs, since the
reset state is highly normal. Instead, the following initial/seed states are provided for the
one- and two-instruction sweep test campaigns:

| Seed | State |
|------|---------------------------------------------------------------------------------|
| `S0` | The canonical reset state described in the ISA manual. |
| `S1` | Distinct register values, and a stack in the middle of memory. |
| `S2` | `S1` with every useful `FLAGS` bit set. |
| `S3` | Register pairs and a stack at the bottom of memory. |
| `S4` | Register pairs and a stack at the top of memory. |
| `S5` | User Mode with an MPU which grants everything. |
| `S6` | User Mode with an MPU which grants instruction fetch and nothing else. |
| `S7` | Inside a system call context. |
| `S8` | Inside an interrupt context. |

### `test_programs` Bringup Programs

The bringup programs in `programs/bringup` are written in MISA assembly, assembled with the MISA
toolchain, and are ultimately intended to be run on real hardware. Each one is run two ways:

1. In lockstep against `misa-spec` from reset to check for mid-program divergences.
2. Run in only the testbench (without `misa-spec`) to check that the extended status of `CAUSE`
   is the correct macro code emitted by the program on successful execution.
   
Each program is run with both the minimal set of `EXTNS` that it requires, and will all extensions
enabled.

The following programs are run:

| Program | Checks |
|-------------|-------------------------------------------------------------------------------------------------------------------------|
| `firsttest` | The shortest program which can halt.  |
| `memtest` | Writes and reads back `0xAA` and `0x55` at every byte below the `text` section.  |
| `synctest` | System calls. |
| `mputest` | MPU configuration and fault conditions checks.  |
| `inttest` | The canonical interrupt handler in the ISA manual.  |

### `test_weighted` Profiles

The weighted programs test uses a set of configuration parameters, which together form a profile.
The configuration values are:

- Program length
- Probability of generating a deliberately faulting instruction
- The value of `EXTNS` on reset
- Whether a function body runs in User Mode
- Whether the memory map has holes in it
- Whether a function body uses the stack, and how many nested calls are performed
- Whether a function makes a system call
- Whether a function writes `EXTNS`

The profiles are:

| Profile | Configuration |
|--------------|-------------------------------------------------------------------------------------|
| `plain` | Machine Mode with a complete memory map that grants every access to every address. |
| `faults` | `plain` with 1/20 instructions intended to cause a fault. |
| `holes` | A memory map with an unmapped region, a region which grants nothing, a read-only region, and an unmapped half of the vector table. |
| `user` | User Mode with an MPU which grants minimal permissions and one overlapping region. |
| `userfaults` | User Mode with memory-access-related faults. |
| `calls` | Subroutines two deep, saving and restoring `RADDR` through the stack. |
| `syscalls` | System calls returning through `RETSC`. |
| `narrow` | Only Dynamic Hardware enabled at reset. |
| `dynamic` | Disabled extensions at runtime. |
