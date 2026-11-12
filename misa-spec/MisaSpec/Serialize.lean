import MisaSpec.State


namespace MisaSpec


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


def parseBool? (s : String) : Option Bool :=
  if      s == "true"  then some true
  else if s == "false" then some false
  else none


def b64Value? (c : Char) : Option Nat :=
  let n := c.toNat
  if      n ≥ 'A'.toNat ∧ n ≤ 'Z'.toNat then some (n - 'A'.toNat)
  else if n ≥ 'a'.toNat ∧ n ≤ 'z'.toNat then some (n - 'a'.toNat + 26)
  else if n ≥ '0'.toNat ∧ n ≤ '9'.toNat then some (n - '0'.toNat + 52)
  else if c == '+' then some 62
  else if c == '/' then some 63
  else none


partial def base64DecodeAux : List Char -> ByteArray -> ByteArray
  | c0 :: c1 :: c2 :: c3 :: rest, acc =>
    let v0  := (b64Value? c0).getD 0
    let v1  := (b64Value? c1).getD 0
    let v2  := (b64Value? c2).getD 0
    let v3  := (b64Value? c3).getD 0
    let acc := acc.push (UInt8.ofNat (v0 * 4 + v1 / 16))
    let acc := if c2 == '=' then acc else acc.push (UInt8.ofNat (v1 % 16 * 16 + v2 / 4))
    let acc := if c3 == '=' then acc else acc.push (UInt8.ofNat (v2 % 4 * 64 + v3))
    base64DecodeAux rest acc
  | _, acc => acc


def base64Decode (s : String) : ByteArray :=
  base64DecodeAux s.toList ByteArray.empty


def State.serialize (q : State) : String :=
  let specLines := String.join (CsrReg.all.map fun c =>
    s!"  {c.name} : 0x{toHex 4 (q.special c).toNat}\n")
  let regLines := String.join (GpReg.all.map fun r =>
    s!"  {r.name} : 0x{toHex 2 (q.register r).toNat}\n")
  s!"halted      : {q.halted}\n" ++
  s!"inSyscall   : {q.inSyscall}\n" ++
  s!"syscallPrivs: 0x{toHex 4 q.syscallPrivs.toNat}\n" ++
  s!"inInterrupt : {q.inInterrupt}\n" ++
  s!"intPending  : {q.intPending}\n" ++
  s!"intRscratch : 0x{toHex 4 q.intRscratch.toNat}\n" ++
  s!"intFlags    : 0x{toHex 4 q.intFlags.toNat}\n" ++
  s!"intPrivs    : 0x{toHex 4 q.intPrivs.toNat}\n" ++
  s!"pc          : 0x{toHex 4 q.pc.toNat}\n" ++
  "special:\n" ++
  specLines ++
  "registers:\n" ++
  regLines ++
  "memory: " ++
  memBase64 q ++ "\n"


structure Parsed where
  sect      : Nat
  scalars   : List (String × String)
  specials  : List (String × String)
  registers : List (String × String)
  memory    : String


def splitFirstColon (s : String) : Option (String × String) :=
  match s.splitOn ":" with
  | []          => none
  | [_]         => none
  | key :: rest => some (key.trim, (String.intercalate ":" rest).trim)


def lookup? (xs : List (String × String)) (k : String) : Option String :=
  (xs.find? (fun p => p.1 == k)).map (fun p => p.2)


def scalarBool (xs : List (String × String)) (k : String) (d : Bool) : Bool :=
  ((lookup? xs k).bind parseBool?).getD d


def scalarWord (xs : List (String × String)) (k : String) (d : SpecWord) : SpecWord :=
  match (lookup? xs k).bind parseNumeral? with
  | some n => BitVec.ofNat 16 n
  | none   => d


def parseStateText (content : String) : Parsed :=
  (content.splitOn "\n").foldl (fun acc raw =>
    let line := raw.trim
    if line.isEmpty then acc
    else match splitFirstColon line with
      | none        => acc
      | some (k, v) =>
        if k == "special" && v.isEmpty then
          { acc with sect := 1 }
        else if k == "registers" && v.isEmpty then
          { acc with sect := 2 }
        else if k == "memory" then
          { acc with memory := v }
        else
          match acc.sect with
          | 1 => { acc with specials  := acc.specials  ++ [(k, v)] }
          | 2 => { acc with registers := acc.registers ++ [(k, v)] }
          | _ => { acc with scalars   := acc.scalars   ++ [(k, v)] })
    ({ sect := 0, scalars := [], specials := [], registers := [], memory := "" } : Parsed)


def State.deserialize (content : String) : Option State := do
  let p := parseStateText content
  let halted ← (lookup? p.scalars "halted").bind parseBool?
  let pcN    ← (lookup? p.scalars "pc").bind parseNumeral?
  let regFn : GpReg -> DataWord := fun r =>
    match r with
    | .r0 => 0
    | _   =>
      match (lookup? p.registers r.name).bind parseNumeral? with
      | some n => BitVec.ofNat 8 n
      | none   => 0
  let specFn : CsrReg -> SpecWord := fun c =>
    match (lookup? p.specials c.name).bind parseNumeral? with
    | some n => BitVec.ofNat 16 n &&& c.writeMask
    | none   => 0
  let bytes := base64Decode p.memory
  let memFn : AddrWord -> DataWord := fun a =>
    if a.toNat < bytes.size then BitVec.ofNat 8 (bytes.get! a.toNat).toNat else 0
  return {
    register     := regFn,
    special      := specFn,
    memory       := memFn,
    check        := CsrFlags.eval (CsrFlags.dec (specFn .flags)),
    pc           := BitVec.ofNat 16 pcN,
    inSyscall    := scalarBool p.scalars "inSyscall"   false,
    inInterrupt  := scalarBool p.scalars "inInterrupt" false,
    intPending   := scalarBool p.scalars "intPending"  false,
    intRscratch  := scalarWord p.scalars "intRscratch"  0,
    intFlags     := scalarWord p.scalars "intFlags"     0,
    syscallPrivs := scalarWord p.scalars "syscallPrivs" 0,
    intPrivs     := scalarWord p.scalars "intPrivs"     0,
    halted       := halted
  }


end MisaSpec
