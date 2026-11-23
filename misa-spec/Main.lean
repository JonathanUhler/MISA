import MisaSpec


open MisaSpec


inductive Emit where
  | final
  | trace


def parseIntSteps? (s : String) : Option (List Nat) :=
  if s == "-" then some [] else (s.splitOn ",").mapM parseNumeral?


def parseLimit? (s : String) : Option (Option Nat) :=
  if s == "-" then some none else s.toNat?.map some


def fireAt (ints : List Nat) (k : Nat) (q : State) : State :=
  if ints.contains k then { q with intPending := true } else q


def runSteps (ints : List Nat) (n : Nat) (k : Nat) (q : State) : State :=
  match n with
  | 0      => q
  | n' + 1 => if q.halted then q else runSteps ints n' (k + 1) (step (fireAt ints k q))


partial def runUntilHalt (ints : List Nat) (k : Nat) (q : State) : State :=
  if q.halted then q else runUntilHalt ints (k + 1) (step (fireAt ints k q))


def runLimit (ints : List Nat) (limit : Option Nat) (q : State) : State :=
  match limit with
  | none   => runUntilHalt ints 0 q
  | some n => runSteps ints n 0 q


partial def traceFrom (ints : List Nat) (limit : Option Nat) (k : Nat) (q : State) : IO Unit := do
  if q.halted || limit == some 0 then
    return
  let q' := step (fireAt ints k q)
  IO.println "---"
  IO.print (State.serializeDelta q q')
  traceFrom ints (limit.map (· - 1)) (k + 1) q'


def runFrom (q0 : State) (stepsArg : String) (ints : List Nat) (emit : Emit) : IO UInt32 := do
  match parseLimit? stepsArg with
  | none       => return 1
  | some limit =>
    match emit with
    | .final => IO.print (State.serialize (runLimit ints limit q0))
    | .trace =>
      IO.print (State.serialize q0)
      traceFrom ints limit 0 q0
    return 0


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


def parsePatch? (s : String) : Option (List (Nat × DataWord)) :=
  match s.splitOn ":" with
  | [addrStr, bytesStr] => do
    let base  ← parseNumeral? (trimmed addrStr)
    let cells := (bytesStr.splitOn " ").map trimmed |>.filter (fun c => !c.isEmpty)
    let bytes ← cells.mapM parseNumeral?
    return bytes.mapIdx (fun i v => (base + i, BitVec.ofNat 8 v))
  | _                   => none


def parseJob? (s : String) : Option (List (Nat × DataWord)) := do
  let groups  := (s.splitOn ";").map trimmed |>.filter (fun g => !g.isEmpty)
  let patches ← groups.mapM parsePatch?
  return patches.flatten


def readJobs (jobsPath : String) : IO (Option (List (List (Nat × DataWord)))) := do
  let content := ← IO.FS.readFile (⟨jobsPath⟩ : System.FilePath)
  let lines   := (content.splitOn "\n").map trimmed |>.filter (fun l => !l.isEmpty)
  let mut jobs : Array (List (Nat × DataWord)) := #[]
  for line in lines do
    match parseJob? line with
    | some job => jobs := jobs.push job
    | none     =>
      IO.eprintln s!"{jobsPath}: malformed job '{line}'"
      return none
  return some jobs.toList


def patchState (q : State) (patches : List (Nat × DataWord)) : State :=
  patches.foldl (fun q (a, v) => q.setMem (BitVec.ofNat 16 a) v) q


partial def runJobs (img ref : State) (ints : List Nat) (limit : Option Nat)
    (jobs : List (List (Nat × DataWord))) : IO Unit := do
  match jobs with
  | []          => return
  | job :: rest =>
    let final := runLimit ints limit (State.start (patchState img job))
    IO.println "---"
    IO.print (State.serializeDelta ref final)
    runJobs img ref ints limit rest


def runQ0 (memPath : String) (stepsArg : String) (extnsArg : String) (intsArg : String)
    (mapPath : String) (emit : Emit) : IO UInt32 := do
  match parseNumeral? extnsArg, parseIntSteps? intsArg with
  | some e, some ints =>
    let bytes ← IO.FS.readBinFile (⟨memPath⟩ : System.FilePath)
    match ← readMemMap mapPath with
    | none     => return 1
    | some map => runFrom (State.first bytes (BitVec.ofNat 16 e) map) stepsArg ints emit
  | _,      _         => return 1


def runQi (statePath : String) (stepsArg : String) (intsArg : String) (emit : Emit) : IO UInt32 := do
  match parseIntSteps? intsArg with
  | none      => return 1
  | some ints =>
    let content ← IO.FS.readFile (⟨statePath⟩ : System.FilePath)
    match State.deserialize content with
    | none   => return 1
    | some q => runFrom q stepsArg ints emit


def runSweep (memPath : String) (stepsArg : String) (extnsArg : String) (intsArg : String)
    (mapPath : String) (jobsPath : String) : IO UInt32 := do
  match parseNumeral? extnsArg, parseIntSteps? intsArg, parseLimit? stepsArg with
  | some e, some ints, some limit =>
    let bytes ← IO.FS.readBinFile (⟨memPath⟩ : System.FilePath)
    match ← readMemMap mapPath with
    | none     => return 1
    | some map =>
      match ← readJobs jobsPath with
      | none      => return 1
      | some jobs =>
        let img := State.init bytes (BitVec.ofNat 16 e) map
        let ref := State.start img
        IO.print (State.serialize ref)
        runJobs img ref ints limit jobs
        return 0
  | _,      _,         _          => return 1


def main (args : List String) : IO UInt32 := do
  match args with
  | ["q0", memPath, stepsArg, extnsArg, intsArg] =>
    runQ0 memPath stepsArg extnsArg intsArg "-" .final
  | ["q0", memPath, stepsArg, extnsArg, intsArg, mapPath] =>
    runQ0 memPath stepsArg extnsArg intsArg mapPath .final
  | ["q0", memPath, stepsArg, extnsArg, intsArg, mapPath, "trace"] =>
    runQ0 memPath stepsArg extnsArg intsArg mapPath .trace
  | ["qi", statePath, stepsArg, intsArg] =>
    runQi statePath stepsArg intsArg .final
  | ["qi", statePath, stepsArg, intsArg, "trace"] =>
    runQi statePath stepsArg intsArg .trace
  | ["sweep", memPath, stepsArg, extnsArg, intsArg, mapPath, jobsPath] =>
    runSweep memPath stepsArg extnsArg intsArg mapPath jobsPath
  | _ => return 1
