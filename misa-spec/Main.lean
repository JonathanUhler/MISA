import MisaSpec


open MisaSpec


def toHex (digits : Nat) (v : Nat) : String :=
  String.mk (List.leftpad digits '0' (Nat.toDigits 16 v))


def b64Digit (n : Nat) : Char :=
  if      n < 26 then Char.ofNat (n + 'A'.toNat)
  else if n < 52 then Char.ofNat (n - 26 + 'a'.toNat)
  else if n < 62 then Char.ofNat (n - 52 + '0'.toNat)
  else if n == 62 then '+'
  else '/'


def memBase64 (q : State) : String :=
  let chars := (List.range 21846).foldl (fun acc g =>
    let i   := g * 3
    let rem := 65536 - i
    let b0  := (q.memory (BitVec.ofNat 16 i)).toNat
    let b1  := if rem ≥ 2 then (q.memory (BitVec.ofNat 16 (i + 1))).toNat else 0
    let b2  := if rem ≥ 3 then (q.memory (BitVec.ofNat 16 (i + 2))).toNat else 0
    let acc := acc.push (b64Digit (b0 / 4))
    let acc := acc.push (b64Digit (b0 % 4 * 16 + b1 / 16))
    let acc := acc.push (if rem ≥ 2 then b64Digit (b1 % 16 * 4 + b2 / 64) else '=')
    acc.push (if rem ≥ 3 then b64Digit (b2 % 64) else '=')) (#[] : Array Char)
  String.mk chars.toList


def digitValue? (c : Char) : Option Nat :=
  let n := c.toNat
  if      n ≥ '0'.toNat ∧ n ≤ '9'.toNat then some (n - '0'.toNat)
  else if n ≥ 'a'.toNat ∧ n ≤ 'f'.toNat then some (n - 'a'.toNat + 10)
  else if n ≥ 'A'.toNat ∧ n ≤ 'F'.toNat then some (n - 'A'.toNat + 10)
  else none


def parseDigits (base : Nat) (cs : List Char) : Option Nat :=
  match cs with
  | [] => none
  | _  => cs.foldl (fun acc c =>
    match acc, digitValue? c with
    | some v, some d => if d < base then some (v * base + d) else none
    | _,      _      => none) (some 0)


def parseNumeral? (s : String) : Option Nat :=
  match s.toList with
  | '0' :: 'x' :: rest => parseDigits 16 rest
  | '0' :: 'X' :: rest => parseDigits 16 rest
  | '0' :: 'b' :: rest => parseDigits 2  rest
  | '0' :: 'B' :: rest => parseDigits 2  rest
  | cs                 => parseDigits 10 cs


def runSteps : Nat -> State -> State
  | 0,     q => q
  | n + 1, q => if q.halted then q else runSteps n (execute q)


partial def runUntilHalt (q : State) : State :=
  if q.halted then q else runUntilHalt (execute q)


def dumpState (q : State) : String :=
  let extns : CsrExtns := CsrExtns.dec (q.special .extns)
  let specLines := String.join ((CsrReg.all.filter (·.availableUnder extns)).map fun c =>
    s!"  {c.name} = 0x{toHex 4 (q.special c).toNat}\n")
  let regLines := String.join (GpReg.all.map fun r =>
    s!"  {r.name} = 0x{toHex 2 (q.register r).toNat}\n")
  let syscallLines :=
    if extns.syscall then s!"inSyscall   = {q.inSyscall}\n" else ""
  let interruptLines :=
    if extns.interrupt then
      s!"inInterrupt = {q.inInterrupt}\n" ++
      s!"intRscratch = 0x{toHex 4 q.intRscratch.toNat}\n" ++
      s!"intFlags    = 0x{toHex 4 q.intFlags.toNat}\n"
    else ""
  s!"halted      = {q.halted}\n" ++
  syscallLines ++
  interruptLines ++
  s!"pc          = 0x{toHex 4 q.pc.toNat}\n" ++
  "special:\n" ++
  specLines ++
  "registers:\n" ++
  regLines ++
  "memory (base64):\n" ++
  memBase64 q ++ "\n"


def runAndDump (bytes : ByteArray) (stepsArg : String) (extns : SpecWord) : IO Unit := do
  let q0 := State.first bytes extns
  if stepsArg == "-" then
    IO.print (dumpState (runUntilHalt q0))
  else
    match stepsArg.toNat? with
    | some n => IO.print (dumpState (runSteps n q0))
    | none   =>
      IO.eprintln s!"error: <steps> must be a non-negative integer or '-', got '{stepsArg}'"


def main (args : List String) : IO Unit := do
  match args with
  | [path, stepsArg] =>
    let bytes ← IO.FS.readBinFile (⟨path⟩ : System.FilePath)
    runAndDump bytes stepsArg 0x0000
  | [path, stepsArg, extnsArg] =>
    match parseNumeral? extnsArg with
    | none   =>
      IO.eprintln s!"error: <extns> must be a number (e.g. 14, 0xE, or 0b1110), got '{extnsArg}'"
    | some e =>
      let bytes ← IO.FS.readBinFile (⟨path⟩ : System.FilePath)
      runAndDump bytes stepsArg (BitVec.ofNat 16 e)
  | _ =>
    IO.eprintln "usage: misa-spec <binary-file> <steps> [extns]"
    IO.eprintln "  <binary-file>  flat memory image to load at address 0x0000"
    IO.eprintln "  <steps>        number of instructions to execute, or '-' to run until halt"
    IO.eprintln "  [extns]        optional EXTNS register value (decimal, 0x…, or 0b…); default 0x0000"
