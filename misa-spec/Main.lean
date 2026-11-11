import MisaSpec


open MisaSpec


def runSteps : Nat -> State -> State
  | 0,     q => q
  | n + 1, q => if q.halted then q else runSteps n (step q)


partial def runUntilHalt (q : State) : State :=
  if q.halted then q else runUntilHalt (step q)


def runFrom (q0 : State) (stepsArg : String) : IO UInt32 := do
  if stepsArg == "-" then
    IO.print (State.serialize (runUntilHalt q0))
    return 0
  else
    match stepsArg.toNat? with
    | some n =>
      IO.print (State.serialize (runSteps n q0))
      return 0
    | none   => return 1


def main (args : List String) : IO UInt32 := do
  match args with
  | ["q0", memPath, stepsArg, extnsArg] =>
    match parseNumeral? extnsArg with
    | none   => return 1
    | some e =>
      let bytes ← IO.FS.readBinFile (⟨memPath⟩ : System.FilePath)
      runFrom (State.first bytes (BitVec.ofNat 16 e)) stepsArg
  | ["qi", statePath, stepsArg] =>
    let content ← IO.FS.readFile (⟨statePath⟩ : System.FilePath)
    match State.deserialize content with
    | none   => return 1
    | some q => runFrom q stepsArg
  | _ => return 1
