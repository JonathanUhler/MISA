import MisaSpec


open MisaSpec


def parseIntSteps? (s : String) : Option (List Nat) :=
  if s == "-" then some [] else (s.splitOn ",").mapM parseNumeral?


def fireAt (ints : List Nat) (k : Nat) (q : State) : State :=
  if ints.contains k then { q with intPending := true } else q


def runSteps (ints : List Nat) (n : Nat) (k : Nat) (q : State) : State :=
  match n with
  | 0      => q
  | n' + 1 => if q.halted then q else runSteps ints n' (k + 1) (step (fireAt ints k q))


partial def runUntilHalt (ints : List Nat) (k : Nat) (q : State) : State :=
  if q.halted then q else runUntilHalt ints (k + 1) (step (fireAt ints k q))


def runFrom (q0 : State) (stepsArg : String) (ints : List Nat) : IO UInt32 := do
  if stepsArg == "-" then
    IO.print (State.serialize (runUntilHalt ints 0 q0))
    return 0
  else
    match stepsArg.toNat? with
    | some n =>
      IO.print (State.serialize (runSteps ints n 0 q0))
      return 0
    | none   => return 1


def readMemMap (mapPath : String) : IO (Option MemMap) := do
  if mapPath == "-" then
    return some []
  else
    let content ← IO.FS.readFile (⟨mapPath⟩ : System.FilePath)
    match parseMemMap content with
    | .ok map    => return some map
    | .error err => do
      IO.eprintln s!"{mapPath}: {err}"
      return none


def runQ0 (memPath : String) (stepsArg : String) (extnsArg : String) (intsArg : String)
    (mapPath : String) : IO UInt32 := do
  match parseNumeral? extnsArg, parseIntSteps? intsArg with
  | some e, some ints =>
    let bytes ← IO.FS.readBinFile (⟨memPath⟩ : System.FilePath)
    match ← readMemMap mapPath with
    | none     => return 1
    | some map => runFrom (State.first bytes (BitVec.ofNat 16 e) map) stepsArg ints
  | _,      _         => return 1


def main (args : List String) : IO UInt32 := do
  match args with
  | ["q0", memPath, stepsArg, extnsArg, intsArg] =>
    runQ0 memPath stepsArg extnsArg intsArg "-"
  | ["q0", memPath, stepsArg, extnsArg, intsArg, mapPath] =>
    runQ0 memPath stepsArg extnsArg intsArg mapPath
  | ["qi", statePath, stepsArg, intsArg] =>
    match parseIntSteps? intsArg with
    | none      => return 1
    | some ints =>
      let content ← IO.FS.readFile (⟨statePath⟩ : System.FilePath)
      match State.deserialize content with
      | none   => return 1
      | some q => runFrom q stepsArg ints
  | _ => return 1
