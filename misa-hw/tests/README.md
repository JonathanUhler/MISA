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

| Option         | Purpose                                                                                |
|----------------|----------------------------------------------------------------------------------------|
| `--target`     | Which target model to run tests against.                                               |
| `--compare`    | Compare every state ("lockstep") or only the final state after a test program is run.  |
| `--latency`    | The memory access latency pattern to use when `MisaCore` performs memory transactions. |
| `--exhaustive` | Whether to run the full number of tests for every campaign, or only a quicker subset.  |
| `--seeds`      | The number of random seeds to use per test campaign.                                                                                       |

The following pytest markers are also available:

| Marker        | Purpose                                                       |
|---------------|---------------------------------------------------------------|
| `subset`      | Part of the Level B and Level B test subset.                  |
| `exhaustive`  | Long tests that are only run when `--exhaustive` is provided. |
| `random`      | Tests with randomized inputs.                                 |
| `equivalence` | Formal verification and netlist equivalence tests.            |
| `slow`        | Long-running tests.                                           |

Some examples of running the tests include:

```
pytest                                   # amaranth level, lockstep, quick campaign sizes
pytest --latency=fixed4                  # present a 4-cycle memory to MisaCore
pytest --latency=random                  # present a memory with random latency to MisaCore
pytest -m subset                         # only run tests in the verilog/gates subset
```
