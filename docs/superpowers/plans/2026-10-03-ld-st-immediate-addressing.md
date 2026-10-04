# LD/ST Immediate Addressing + Stack Mode Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give `LD`/`ST` a signed 4-bit immediate displacement and a reserved stack-addressing mode, collapsing `PUSH`/`POP`/`PUSH2`/`POP2` to one or two real instructions.

**Architecture:** `LD RD RSB IMM` / `ST RD RSB IMM` keep opcodes `0xA`/`0xB` but reinterpret the last nibble: `nib2` is the base register (the **high** byte of the address; the low byte is the adjacent register `RSB+1`) and `nib3` is a signed 4-bit two's-complement offset (−8..7). The base encoding `0xF` (`RSCRATCH1`) can never form a valid pair (its partner would be register `0x10`), so it is reserved as a stack mode that operates on `SADDR` directly. The `LD`/`ST` AST node is shared across `misa-as` and `misa-objdump` (objdump depends on the misa-as library), so the grammar, parser, packer, decoder, printer and simulator all change atomically. A separate, independent change teaches the assembler's general integer parser to accept signed literals.

**Tech Stack:** Haskell (Stack, Megaparsec) for `misa-as`/`misa-objdump`; Python for the reference simulator (`misa-sim/simulator.py`) and the pytest conformance suite under `tests/conformance`.

**Spec:** `docs/arch/isa.qmd` — the `LD`, `ST`, `POP`, `POP2`, `PUSH`, `PUSH2` sections and the General Purpose Registers / Stack Conventions sections. Task 1 finalizes that spec to match the decisions below.

## Global Constraints

- **Pair convention:** the named base register `RSB` is the **high** byte; the low byte is register `RSB+1`. Address = `(Register[RSB] << 8) | Register[RSB+1]`. This matches the wide-register ABI (`RAB`=`RA,RB`=high,low).
- **Stack marker:** base encoding `0xF` (`RSCRATCH1`) is reserved for stack mode and never forms a register pair. In stack mode the register file value of `RSCRATCH1` is irrelevant — only `SADDR` is used.
- **Offset field:** signed 4-bit two's complement, range −8..7, stored in `nib3`. The assembler accepts **signed decimal only** for this field (no hex/bin/oct), so `0xF` can never be mistaken for +15.
- **Stack semantics:** `LD` stack mode pre-adjusts (`SADDR += IMM; RD = Memory[SADDR]`); `ST` stack mode post-adjusts (`Memory[SADDR] = RD; SADDR += IMM`).
- **Stack byte order (unchanged from today):** `PUSH2 RS1 RS2` pushes `RS2` then `RS1`; `POP2 RD1 RD2` pops `RD1` then `RD2`.
- **General integer literals:** `parseInteger` accepts an optional leading `-` or `+` on every base; all existing unsigned forms keep working. A word literal accepts −128..255, a doubleword −32768..65535, each wrapped to two's complement.
- **Address/SADDR arithmetic wraps mod 2^16** (existing `ADDR_MASK`/`CSR_MASK` masking), never faults on a displaced address.
- **Build/test:** after any Haskell change run `make` from the repo root before the conformance suite — the suite shells out to `install/misa-as` and `install/misa-objdump`. The simulator is imported live from `misa-sim/simulator.py`, so simulator edits need no rebuild. Run the suite with `pytest tests/conformance`.
- **No git commits.** Per project convention, leave all changes unstaged for the user to review; do not `git add`/`commit`/`push`. Each task ends by running its tests and the full suite, then stopping for review.

## Review Focus

- **Offset boundaries and overflow:** `IMM` = −8 and 7 must encode/sign-extend correctly (`nib3` 0x8 and 0x7); `-9`/`8` must be rejected by the assembler with a clear range error. → Task 3 tests.
- **Address/SADDR wrap:** a negative offset at address 0, or any offset crossing 0x0000/0xFFFF, must wrap, not crash. → Task 3 test.
- **The register adjacent to the marker:** base `RSCRATCH0` (`0xE`) is a *normal* load whose low byte is `RSCRATCH1` (`0xF`); only base `0xF` is the stack marker. → Task 3 test.
- **Disassembly re-parses:** objdump must print the offset as signed decimal (`-1`, `0`, `3`) so the output re-assembles. → Task 3 objdump test.
- **Non-canonical legacy pairs:** migrating `ld rd rHI rLO` to `ld rd rHI 0` is only correct when `rLO == rHI+1`; a reversed/non-adjacent pair would silently change the address. Every migrated site must be checked for adjacency. → Task 3 & Task 5 migration steps (all current sites are canonical; verify).

---

### Task 1: Finalize the ISA spec (`isa.qmd`)

Bring the manual in line with the agreed design before any code is written. The current unstaged diff has the pre-decision wording (`{RSL+1, RSL}`, `RSL`, and the reversed PUSH2/POP2 order); fix those.

**Files:**
- Modify: `docs/arch/isa.qmd` — `LD` (~lines 724–761), `ST` (~764–801), `POP2` (~1118), `PUSH2` (~1139), and the legend entry for `SignExtN` (~444).

- [ ] **Step 1: Rename the operand and flip the pair order in `LD`**

In the `LD` section change the usage, description, encoding diagram field, and Meaning so the named register is the high byte:

- Usage: `LD RD RSB IMM`
- Description: "Load the 8-bit word at the address formed by `{RSB, RSB + 1} + IMM` into `RD`. The base address is stored in two adjacent general purpose registers; the instruction names the **high** register `RSB`, and the low byte is taken from register `RSB + 1`."
- Keep the sentence reserving `0xF` for stack operations and the sentence describing the 4-bit signed offset.
- Encoding diagram: rename the `RSL` field to `RSB` (leave `IMM` as the 4-bit field).
- Meaning:

```
If (RSB != 0xF) {
  Register[RD] = Memory[((Register[RSB] << 0x08) | Register[RSB + 1]) + SignExt16 IMM];
}
If (RSB == 0xF) {
  Special[SADDR] = Special[SADDR] + SignExt16 IMM;
  Register[RD] = Memory[Special[SADDR]];
}
```

- [ ] **Step 2: Apply the same rename/flip to `ST`**

- Usage: `ST RD RSB IMM`
- Description mirrors `LD` ("names the high register `RSB`, low byte is register `RSB + 1`").
- Encoding diagram: rename `RSL` → `RSB`.
- Meaning:

```
If (RSB != 0xF) {
  Memory[((Register[RSB] << 0x08) | Register[RSB + 1]) + SignExt16 IMM] = Register[RD];
}
If (RSB == 0xF) {
  Memory[Special[SADDR]] = Register[RD];
  Special[SADDR] = Special[SADDR] + SignExt16 IMM;
}
```

- [ ] **Step 3: Revert PUSH2/POP2 to the existing stack byte order**

`POP2` Meaning (pop high first, then low):

```
LD RD1 RSCRATCH1 +1
LD RD2 RSCRATCH1 +1
```

`PUSH2` Meaning (push low first, then high):

```
ST RS2 RSCRATCH1 -1
ST RS1 RSCRATCH1 -1
```

Leave `POP`/`PUSH` as the single-instruction forms already in the diff (`LD RD RSCRATCH1 +1` / `ST RS RSCRATCH1 -1`).

- [ ] **Step 4: Verify the manual renders**

Run: `make docs` (renders `docs/arch` to PDF via quarto).
Expected: render completes without error and the `LD`/`ST`/`PUSH2`/`POP2` sections read as above. If `quarto` is unavailable in the environment, instead re-read the four sections and the legend to confirm consistency (no remaining `RSL`, no `{RSL+1, RSL}`, PUSH2/POP2 match Step 3).

- [ ] **Step 5: Leave unstaged for review**

Do not commit. Leave `docs/arch/isa.qmd` modified in the working tree.

---

### Task 2: Signed integer literals in the assembler

Independent, self-contained change (no AST change, compiles on its own). Teaches the general integer parser to accept an optional leading sign, fixing the long-standing "misa-as rejects negative decimals" gap while keeping every current unsigned form.

**Files:**
- Modify: `misa-as/src/Parser.hs` — `parseInteger` (~76–81), `parseWord` (~84–90), `parseDoubleWord` (~93–99).
- Create: `tests/conformance/instructions/test_signed_literals.py`

**Interfaces:**
- Produces: `parseInteger :: Parser Int` now returns possibly-negative values; `parseWord :: Parser Word8` / `parseDoubleWord :: Parser Word16` accept signed input and wrap via `fromIntegral`.

- [ ] **Step 1: Write the failing tests**

Create `tests/conformance/instructions/test_signed_literals.py`:

```python
"""
Conformance tests for signed integer literals in the assembler.

Negative literals are accepted on any base and wrap to two's complement. All existing unsigned
forms continue to work.
"""


from harness import run, Reg


def test_set_accepts_negative_decimal():
    """`set ra -1` wraps to 0xFF."""

    sim = run("""
        set ra -1
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0xFF


def test_set_accepts_negative_hex():
    """`set ra -0x10` wraps to 0xF0 (−16)."""

    sim = run("""
        set ra -0x10
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0xF0


def test_set2_accepts_negative_doubleword():
    """`set2 ra rb -2` loads 0xFFFE: ra (high) = 0xFF, rb (low) = 0xFE."""

    sim = run("""
        set2 ra rb -2
        halt r0
    """)
    assert sim.reg[Reg.RA] == 0xFF
    assert sim.reg[Reg.RB] == 0xFE


def test_positive_literals_still_work():
    """Unsigned forms are unaffected."""

    sim = run("""
        set ra 0x2A
        halt ra
    """)
    assert sim.reg[Reg.RA] == 0x2A
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `make && pytest tests/conformance/instructions/test_signed_literals.py -v`
Expected: the three negative-literal tests FAIL (assembly error: `L.decimal` cannot parse a leading `-`); `test_positive_literals_still_work` passes.

- [ ] **Step 3: Add optional sign to `parseInteger`**

Replace `parseInteger` in `misa-as/src/Parser.hs`:

```haskell
parseInteger :: Parser Int
parseInteger = lexeme $ do
  sign <- option id (negate <$ char '-' <|> id <$ char '+')
  mag  <- try parseHex <|> try parseBin <|> try parseOct <|> parseDec
  return (sign mag)
  where parseHex = parseString "0x" *> L.hexadecimal
        parseBin = parseString "0b" *> L.binary
        parseOct = parseString "0o" *> L.octal
        parseDec = L.decimal
```

(`char` and `(<|>)` are already in scope via `Text.Megaparsec.Char` / `Text.Megaparsec`; `option` is from `Text.Megaparsec`.)

- [ ] **Step 4: Relax the range checks to accept signed values**

In `parseWord`, change the bounds check so a signed byte is accepted and wrapped:

```haskell
parseWord :: Parser Word8
parseWord = do
  int <- parseInteger <?> "integer literal"
  if int < -128 || int > fromIntegral (maxBound :: Word8) then
    fail ("integer literal " ++ show int ++ " is not representable as a word")
  else
    return (fromIntegral int)
```

In `parseDoubleWord`:

```haskell
parseDoubleWord :: Parser Word16
parseDoubleWord = do
  int <- parseInteger <?> "integer literal"
  if int < -32768 || int > fromIntegral (maxBound :: Word16) then
    fail ("integer literal " ++ show int ++ " is not representable as a double-word")
  else
    return (fromIntegral int)
```

(`fromIntegral (-1 :: Int) :: Word8` is `0xFF`, so negatives wrap correctly.)

- [ ] **Step 5: Rebuild and run the tests to verify they pass**

Run: `make && pytest tests/conformance/instructions/test_signed_literals.py -v`
Expected: all four tests PASS.

- [ ] **Step 6: Run the full suite; leave unstaged**

Run: `pytest tests/conformance`
Expected: no new failures. Leave changes unstaged (do not commit).

---

### Task 3: New LD/ST immediate addressing + stack mode (atomic toolchain change)

Changes the shared `LD`/`ST` AST, so the misa-as grammar/parser/packer, the misa-objdump decoder/printer, and the simulator all change together (nothing compiles until they agree). Also migrates every suite-assembled site that used the old three-register syntax so the suite stays green.

**Files:**
- Modify: `misa-as/src/Grammar.hs` — `LdInst`/`StInst` constructors (~50–51).
- Modify: `misa-as/src/Parser.hs` — add `parseOffset`, `parseBaseReg`; rewrite the `ld`/`st` entries in `coreInsts` (~213–214).
- Modify: `misa-as/src/ObjectFile.hs` — `packInst` LD/ST cases and a new `packFormatRRI4` helper (~265–266, 276–282); add `(.&.)` to the `Data.Bits` import (~37).
- Modify: `misa-objdump/src/Decoder.hs` — LD/ST cases + a `signExt4` helper (~195–196).
- Modify: `misa-objdump/src/Printer.hs` — LD/ST cases + a `showOffset` helper (~41–42).
- Modify: `misa-sim/simulator.py` — `_ld`/`_st` (~414–421).
- Modify (migrate to new syntax): `tests/conformance/instructions/test_ld.py`, `tests/conformance/instructions/test_st.py`, `tests/conformance/instructions/test_pop.py`, `tests/conformance/test_linker_placement.py`, `tests/conformance/programs/loadstore.S`, `tests/conformance/programs/interrupt_drain.S`, `demos/echo/echo.S`.
- Create: `tests/conformance/instructions/test_ld_immediate.py`, `tests/conformance/test_objdump_ld_st.py`.

**Interfaces:**
- Produces (Haskell AST): `LdInst :: GpReg -> GpReg -> Int -> Inst`, `StInst :: GpReg -> GpReg -> Int -> Inst` — (destination/source register, base register = high byte, signed offset −8..7).
- Produces (parser): `parseOffset :: Parser Int` (signed decimal, range-checked −8..7); `parseBaseReg :: Parser GpReg` (a wide-register name resolves to its high/first register, or a single GP register).
- Produces (simulator): `_ld(self, rd, base, off)` / `_st(self, rd, base, off)` where `off` is the raw 0–15 nibble (sign-extended internally); dispatch in `step()` is unchanged (`self._ld(nib1, nib2, nib3)`).

- [ ] **Step 1: Write the failing behavioral tests**

Create `tests/conformance/instructions/test_ld_immediate.py`:

```python
"""
Conformance tests for LD/ST immediate addressing and stack mode.

`LD RD RSB IMM` / `ST RD RSB IMM`: the named base register RSB is the high byte of the address,
the low byte is register RSB+1, and IMM is a signed 4-bit displacement (-8..7). Base encoding 0xF
(RSCRATCH1) is reserved for stack mode, which pre-adjusts SADDR on load and post-adjusts on store.
"""


import pytest
from harness import run, Reg, Csr


def test_ld_base_names_high_register():
    """Naming RA reads {RA, RB} = high:low, matching the wide-register ABI."""

    sim = run("""
        set ra 0x02
        set rb 0x00
        set rc 0x2A
        st rc ra 0
        ld rd ra 0
        halt rd
    """)
    assert sim.reg[Reg.RD] == 0x2A


def test_ld_applies_positive_offset():
    """A +1 offset reads the byte after the base address."""

    sim = run(
        """
        set2 ra rb value
        ld rc ra 1
        halt rc
        """,
        data="""
        value:
            .word 0x2A
            .word 0x3B
        """,
    )
    assert sim.reg[Reg.RC] == 0x3B


def test_ld_applies_negative_offset():
    """A -1 offset reads the byte before the base address."""

    sim = run(
        """
        set2 ra rb value
        ld rc ra -1
        halt rc
        """,
        data="""
            .word 0x11
        value:
            .word 0x2A
        """,
    )
    assert sim.reg[Reg.RC] == 0x11


def test_negative_offset_wraps_without_faulting():
    """An offset that pushes the address below 0 wraps mod 2^16 instead of faulting."""

    sim = run("""
        set ra 0x00
        set rb 0x00
        set rc 0x2A
        st rc ra -1
        ld rd ra -1
        halt rd
    """)
    assert sim.reg[Reg.RD] == 0x2A


def test_offset_boundaries_encode():
    """Offsets 7 and -8 are the field extremes and round-trip through the simulator."""

    sim = run(
        """
        set2 ra rb base
        ld rc ra 7
        ld rd ra -8
        halt r0
        """,
        data="""
            .space 8
        base:
            .space 8
        """,
    )
    # base+7 and base-8 both land in the seeded .space region without faulting
    assert sim.reg[Reg.RC] == 0x00
    assert sim.reg[Reg.RD] == 0x00


def test_offset_out_of_range_is_rejected():
    """The assembler rejects an offset outside -8..7."""

    with pytest.raises(AssertionError):
        run("""
            ld rc ra 8
            halt rc
        """)


def test_base_rscratch0_is_a_normal_load():
    """Base 0xE (RSCRATCH0) is normal; its low byte is RSCRATCH1 (0xF), not the stack marker."""

    sim = run("""
        set rscratch0 0x02
        set rscratch1 0x00
        set rc 0x2A
        st rc rscratch0 0
        ld rd rscratch0 0
        halt rd
    """)
    assert sim.reg[Reg.RD] == 0x2A


def test_ld_stack_mode_pre_increments_saddr():
    """Base 0xF on LD increments SADDR then reads (POP semantics)."""

    sim = run("""
        set ra 0x01
        set rb 0xFF
        set rc 0x2A
        st rc ra 0
        set ra 0x01
        set rb 0xFE
        wsr saddr ra rb
        ld rd rscratch1 1
        halt rd
    """)
    assert sim.reg[Reg.RD] == 0x2A
    assert sim.get_csr(Csr.SADDR) == 0x01FF


def test_st_stack_mode_post_decrements_saddr():
    """Base 0xF on ST stores then decrements SADDR (PUSH semantics)."""

    sim = run("""
        set ra 0x01
        set rb 0xFF
        wsr saddr ra rb
        set rc 0x2A
        st rc rscratch1 -1
        halt r0
    """)
    assert sim.mem[0x01FF] == 0x2A
    assert sim.get_csr(Csr.SADDR) == 0x01FE
```

- [ ] **Step 2: Run to verify they fail**

Run: `make && pytest tests/conformance/instructions/test_ld_immediate.py -v`
Expected: build FAILS or tests FAIL — the assembler/simulator do not yet understand `ld rc ra 0`. (A build failure here is expected before Steps 3–8 land; that is the "red".)

- [ ] **Step 3: Change the AST**

In `misa-as/src/Grammar.hs`, change the two constructors:

```haskell
  | LdInst   GpReg   GpReg Int
  | StInst   GpReg   GpReg Int
```

- [ ] **Step 4: Update the parser**

In `misa-as/src/Parser.hs`, add the offset and base parsers (place near `parseRegPair`):

```haskell
parseOffset :: Parser Int
parseOffset = lexeme $ do
  sign <- option id (negate <$ char '-' <|> id <$ char '+')
  n    <- L.decimal
  let v = sign n
  if v < -8 || v > 7 then
    fail ("memory offset " ++ show v ++ " is out of range (-8 to 7)")
  else
    return v

parseBaseReg :: Parser GpReg
parseBaseReg = try (fst <$> parseWideReg) <|> parseGpReg
```

Replace the `ld`/`st` entries in `coreInsts` (they no longer use `parseRegPair`):

```haskell
    LdInst <$> (parseThisIdent "ld" *> parseGpReg) <*> parseBaseReg <*> parseOffset,
    StInst <$> (parseThisIdent "st" *> parseGpReg) <*> parseBaseReg <*> parseOffset,
```

- [ ] **Step 5: Update the packer**

In `misa-as/src/ObjectFile.hs`, add `(.&.)` to the imports:

```haskell
import Data.Bits ((.|.), (.&.), shiftR, shiftL)
```

Change the LD/ST cases in `packInst`:

```haskell
    LdInst   rd   base off -> packFormatRRI4 0xA rd base off
    StInst   rd   base off -> packFormatRRI4 0xB rd base off
```

Add the format helper alongside the other `packFormat*` helpers:

```haskell
    packFormatRRI4 op r1 r2 i =
      [op .|. shiftL (fromReg r1) 4, fromReg r2 .|. shiftL (fromIntegral (i .&. 0xF)) 4]
```

- [ ] **Step 6: Update the objdump decoder**

In `misa-objdump/src/Decoder.hs`, change the LD/ST cases and add a sign-extension helper in the `where` block:

```haskell
      0xA                           -> Just (LdInst (toEnum nib1) (toEnum nib2) (signExt4 nib3))
      0xB                           -> Just (StInst (toEnum nib1) (toEnum nib2) (signExt4 nib3))
```

```haskell
    signExt4 n = if n >= 8 then n - 16 else n
```

(`nib3` is already `fromIntegral (hi \`shiftR\` 4) :: Int`.)

- [ ] **Step 7: Update the objdump printer**

In `misa-objdump/src/Printer.hs`, change the LD/ST cases and add a signed-decimal formatter:

```haskell
          LdInst   rd  base off -> unwords ["LD",   show rd,  show base, showOffset off]
          StInst   rd  base off -> unwords ["ST",   show rd,  show base, showOffset off]
```

Add near `showHexInt`:

```haskell
showOffset :: Int -> String
showOffset = show
```

(`show (-1 :: Int)` is `"-1"`, which `parseOffset` re-parses — the round-trip holds.)

- [ ] **Step 8: Update the simulator**

In `misa-sim/simulator.py`, replace `_ld` and `_st`:

```python
    def _ld(self, rd: int, base: int, off: int) -> None:
        off = off - 0x10 if (off & 0x08) else off
        if (base == Reg.RSCRATCH1):
            self.set_csr(Csr.SADDR, self.get_csr(Csr.SADDR) + off)
            self.set_reg(rd, self.read_mem(self.get_csr(Csr.SADDR)))
        else:
            address: int = (self.get_reg(base) << WORD_SIZE) | self.get_reg(base + 1)
            self.set_reg(rd, self.read_mem(address + off))


    def _st(self, rd: int, base: int, off: int) -> None:
        off = off - 0x10 if (off & 0x08) else off
        if (base == Reg.RSCRATCH1):
            self.write_mem(self.get_csr(Csr.SADDR), self.get_reg(rd))
            self.set_csr(Csr.SADDR, self.get_csr(Csr.SADDR) + off)
        else:
            address: int = (self.get_reg(base) << WORD_SIZE) | self.get_reg(base + 1)
            self.write_mem(address + off, self.get_reg(rd))
```

(`read_mem`/`write_mem` mask with `ADDR_MASK`, and `set_csr` masks with `CSR_MASK`, so displaced/ wrapped addresses are handled. `base` is 0–0xE in the else branch, so `base + 1` is always a valid register index.)

- [ ] **Step 9: Rebuild and run the new behavioral tests**

Run: `make && pytest tests/conformance/instructions/test_ld_immediate.py -v`
Expected: all tests PASS.

- [ ] **Step 10: Migrate the suite-assembled LD/ST sites to the new syntax**

These all use canonical `(high, high+1)` pairs; rewrite `<op> RD RHI RLO` → `<op> RD RHI 0` (drop the low register, add the explicit `0` offset). Verify in each case that the dropped register is exactly `RHI+1`.

- `tests/conformance/instructions/test_ld.py`: `ld rc ra rb` → `ld rc ra 0`; update the docstring `Manual:` line to `LD RD RSB IMM` wording.
- `tests/conformance/instructions/test_st.py`: `st rc ra rb` → `st rc ra 0`; update the docstring.
- `tests/conformance/instructions/test_pop.py`: the seed line `st rc ra rb` → `st rc ra 0`.
- `tests/conformance/test_linker_placement.py`: `ld rt rscratch` (two occurrences) → `ld rt rscratch 0`; `st ra rc rd` → `st ra rc 0`.
- `tests/conformance/programs/loadstore.S`: `st rc ra rb` → `st rc ra 0`; `ld rd ra rb` → `ld rd ra 0`.
- `tests/conformance/programs/interrupt_drain.S`: each `ld ra rc rd`/`st ra rc rd`/`st rb rc rd`/`ld rb rc rd`/`st r0 rc rd` → same with the third operand replaced by `0` (`ld ra rc 0`, `st ra rc 0`, `st rb rc 0`, `ld rb rc 0`, `st r0 rc 0`).
- `demos/echo/echo.S`: every `ld ra re rf` → `ld ra re 0`; every `st ra rc rd` → `st ra rc 0`; every `st ra re rf` → `st ra re 0`; `ld ra rc rd` → `ld ra rc 0`. (Grep the file for `\b(ld|st)\b` and convert each `(hi lo)` pair, confirming `lo == hi+1`.)

- [ ] **Step 11: Write the objdump decode test**

Create `tests/conformance/test_objdump_ld_st.py`:

```python
"""
objdump conformance: LD/ST disassemble with the base register and a signed-decimal offset.

Text is placed at 0x0000 by the default memory map, so the first decoded instructions are the ones
under test. The signed-decimal rendering is what makes the disassembly re-assemblable.
"""


import subprocess
import tempfile

from harness import assemble, INSTALL_DIR, _toolchain_env


def _disassemble_all(bin_path: str) -> str:
    result = subprocess.run(
        [str(INSTALL_DIR / "misa-objdump"), "-D", bin_path],
        env=_toolchain_env(),
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    return result.stdout


def test_objdump_renders_base_and_signed_offset():
    with tempfile.TemporaryDirectory() as workdir:
        src = (
            "        .section text\n"
            "_start:\n"
            "        ld rc ra -1\n"
            "        st rd re 3\n"
            "        halt r0\n"
        )
        bin_path = assemble(src, workdir)
        out = _disassemble_all(str(bin_path))
    assert "LD RC RA -1" in out
    assert "ST RD RE 3" in out
```

- [ ] **Step 12: Run the objdump test**

Run: `pytest tests/conformance/test_objdump_ld_st.py -v`
Expected: PASS (build already current from Step 9; if not, `make` first).

- [ ] **Step 13: Run the full suite; leave unstaged**

Run: `pytest tests/conformance`
Expected: green. Leave all changes unstaged (do not commit).

---

### Task 4: Collapse PUSH/POP/PUSH2/POP2 to real LD/ST

With Task 3's `LD`/`ST` in place, rewrite the pseudo-instruction expansions. The new forms use `RSCRATCH1` (0xF) only as the stack marker, so — unlike today's expansions — they no longer clobber `RSCRATCH0`/`RSCRATCH1`.

**Files:**
- Modify: `misa-as/src/Encoder.hs` — `PopInst`/`Pop2Inst`/`PushInst`/`Push2Inst` cases in `resolvePseudoInst` (~68–102).
- Modify: `tests/conformance/instructions/test_push.py`, `test_pop.py`, `test_push2.py`, `test_pop2.py` — docstrings only (behavior is preserved).
- Create: `tests/conformance/instructions/test_stack_preserves_scratch.py`

**Interfaces:**
- Consumes: `LdInst GpReg GpReg Int`, `StInst GpReg GpReg Int` from Task 3; `RSCRATCH1 :: GpReg`.

- [ ] **Step 1: Write the failing test (scratch preservation)**

Create `tests/conformance/instructions/test_stack_preserves_scratch.py`:

```python
"""
The single-instruction PUSH/POP use RSCRATCH1 only as an encoding marker, so they must not disturb
the RSCRATCH register pair (the old multi-instruction expansions did).
"""


from harness import run, Reg


def test_push_preserves_scratch_registers():
    sim = run("""
        set ra 0x01
        set rb 0xFF
        wsr saddr ra rb
        set rscratch0 0xAB
        set rscratch1 0xCD
        set rc 0x2A
        push rc
        halt r0
    """)
    assert sim.reg[Reg.RSCRATCH0] == 0xAB
    assert sim.reg[Reg.RSCRATCH1] == 0xCD
```

- [ ] **Step 2: Run to verify it fails**

Run: `make && pytest tests/conformance/instructions/test_stack_preserves_scratch.py -v`
Expected: FAIL — the current `PushInst` expansion overwrites `RSCRATCH0`/`RSCRATCH1`.

- [ ] **Step 3: Rewrite the expansions**

In `misa-as/src/Encoder.hs` `resolvePseudoInst`, replace the four cases:

```haskell
  PopInst rd
    -> [LdInst rd RSCRATCH1 1]
  Pop2Inst rd1 rd2
    -> [LdInst rd1 RSCRATCH1 1,
        LdInst rd2 RSCRATCH1 1]
  PushInst rs
    -> [StInst rs RSCRATCH1 (-1)]
  Push2Inst rs1 rs2
    -> [StInst rs2 RSCRATCH1 (-1),
        StInst rs1 RSCRATCH1 (-1)]
```

- [ ] **Step 4: Rebuild and run the scratch test**

Run: `make && pytest tests/conformance/instructions/test_stack_preserves_scratch.py -v`
Expected: PASS.

- [ ] **Step 5: Confirm existing stack behavior is preserved**

Run: `pytest tests/conformance/instructions/test_push.py tests/conformance/instructions/test_pop.py tests/conformance/instructions/test_push2.py tests/conformance/instructions/test_pop2.py -v`
Expected: all PASS unchanged (SADDR movement and `push2`/`pop2` byte order are identical to before).

- [ ] **Step 6: Update the pseudo-op docstrings**

In the four test files, update the `Manual:` lines to the new expansions: `POP RD` → `LD RD RSCRATCH1 +1`; `POP2 RD1 RD2` → `LD RD1 RSCRATCH1 +1` then `LD RD2 RSCRATCH1 +1`; `PUSH RS` → `ST RS RSCRATCH1 -1`; `PUSH2 RS1 RS2` → `ST RS2 RSCRATCH1 -1` then `ST RS1 RSCRATCH1 -1`. (These are comments only; no assertion changes.)

- [ ] **Step 7: Run the full suite; leave unstaged**

Run: `pytest tests/conformance`
Expected: green. Leave changes unstaged (do not commit).

---

### Task 5: Migrate remaining (non-suite) assembly sources

`programs/` and `demos/raytrace/` are not exercised by the automated suite but still use the old three-register `LD`/`ST` syntax and must be converted so they assemble under the new toolchain.

**Files:**
- Modify: `programs/arraysum.S` (`ld rt ruv` → `ld rt ruv 0`), `programs/memory.S` (`st ra rscratch` → `st ra rscratch 0`, `ld rb rscratch` → `ld rb rscratch 0`), `demos/raytrace/raytrace.S` (every `ld`/`st` with a register pair → base + `0`).

- [ ] **Step 1: Convert the sources**

For each `ld`/`st` occurrence, drop the low register and append ` 0`, confirming the dropped register is the base's `+1` neighbour (all current sites use canonical `re rf`, `ru rv`/`ruv`, `rscratch`/`rscratch0 rscratch1` pairs). Wide-register names (`ruv`, `rscratch`) may stay as the base and resolve to their high register. Grep each file with `grep -nE '\b(ld|st)\b' <file>` and convert every hit.

- [ ] **Step 2: Verify each program assembles**

Run, for each file, through the installed assembler, e.g.:

```bash
install/misa-as programs/arraysum.S -o /tmp/arraysum.bin
install/misa-as programs/memory.S  -o /tmp/memory.bin
install/misa-as demos/raytrace/raytrace.S -o /tmp/raytrace.bin
```

Expected: each exits 0 with no parse/encoding error. (If `raytrace.S` needs assembler extensions, pass the same `-e` flags its build normally uses.)

- [ ] **Step 3: Run the full suite one last time; leave unstaged**

Run: `pytest tests/conformance`
Expected: green. Leave all changes unstaged for the user to review (do not commit).
