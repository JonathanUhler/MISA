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
| `--compare` | Compare every state ("lockstep") or only the final state after a test program is run.  |
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

### `test_units` Reference Model

`test_units.py` is the only campaign that does not use `misa-spec` as a golden model, because the
units have no architectural state in common with the `misa-spec` state files. Its uses a separate
reference model written from the ISA manual.

### `test_sweep` Seeds

Executing one instruction from the canonical reset state is unlikely to catch many bugs, since the
reset state is highly normal. Instead, the following initial/seed states are provided for the
one-instruction sweep test campaign:

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
