import MisaSpec


open MisaSpec


def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + 48) else Char.ofNat (n + 87)


def toHexAux : Nat -> Nat -> List Char -> List Char
  | 0,      _, acc => acc
  | d + 1,  v, acc => toHexAux d (v / 16) (hexDigit (v % 16) :: acc)


def toHex (digits : Nat) (v : Nat) : String :=
  String.mk (toHexAux digits v [])


def runSteps : Nat -> State -> State
  | 0,     q => q
  | n + 1, q => if q.halted then q else runSteps n (execute q)


partial def runUntilHalt (q : State) : State :=
  if q.halted then q else runUntilHalt (execute q)


def dumpState (q : State) : String :=
  let specLines := String.join (CsrReg.all.map fun c =>
    s!"  {c.name} = 0x{toHex 4 (q.special c).toNat}\n")
  let regLines := String.join (GpReg.all.map fun r =>
    s!"  {r.name} = 0x{toHex 2 (q.register r).toNat}\n")
  s!"halted = {q.halted}\n" ++
  s!"pc     = 0x{toHex 4 q.pc.toNat}\n" ++
  "special:\n" ++
  specLines ++
  "registers:\n" ++
  regLines


def runAndDump (bytes : ByteArray) (stepsArg : String) : IO Unit := do
  let q0 := State.first bytes
  if stepsArg == "*" then
    IO.print (dumpState (runUntilHalt q0))
  else
    match stepsArg.toNat? with
    | some n => IO.print (dumpState (runSteps n q0))
    | none   =>
      IO.eprintln s!"error: <steps> must be a non-negative integer or '*', got '{stepsArg}'"


def main (args : List String) : IO Unit := do
  match args with
  | [path, stepsArg] =>
    let bytes ← IO.FS.readBinFile (⟨path⟩ : System.FilePath)
    runAndDump bytes stepsArg
  | _ =>
    IO.eprintln "usage: misa-spec <binary-file> <steps>"
    IO.eprintln "  <binary-file>  flat memory image to load at address 0x0000"
    IO.eprintln "  <steps>        number of instructions to execute, or '*' to run until halt"
