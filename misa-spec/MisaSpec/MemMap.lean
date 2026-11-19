import MisaSpec.Basic
import Std.Internal.Parsec


namespace MisaSpec


structure MemCaps where
  r : Bool
  w : Bool
  x : Bool
  deriving DecidableEq, BEq, Repr


def MemCaps.rwx : MemCaps := { r := true, w := true, x := true }


def MemCaps.empty : MemCaps := { r := false, w := false, x := false }


def MemCaps.name (c : MemCaps) : String :=
  let s := (if c.r then "r" else "") ++ (if c.w then "w" else "") ++ (if c.x then "x" else "")
  if s.isEmpty then "-" else s


def MemCaps.ofString (s : String) : MemCaps :=
  { r := s.contains 'r', w := s.contains 'w', x := s.contains 'x' }


def inVectorTable (addr : AddrWord) : Bool :=
  0xFFF0#16 <= addr


structure MemRegion where
  lo   : AddrWord
  hi   : AddrWord
  caps : MemCaps
  deriving Repr


abbrev MemMap := List MemRegion


def MemMap.lookup (m : MemMap) (a : AddrWord) : Option MemCaps :=
  (m.find? (fun reg => decide (reg.lo <= a) && decide (a <= reg.hi))).map MemRegion.caps


def MemMap.toCaps (m : MemMap) : AddrWord -> Option MemCaps :=
  match m with
  | [] => fun _ => some MemCaps.rwx
  | _  => m.lookup


section Parser

open Std.Internal.Parsec Std.Internal.Parsec.String


private def hexValue? (c : Char) : Option Nat :=
  let n := c.toNat
  if      n >= '0'.toNat /\ n <= '9'.toNat then some (n - '0'.toNat)
  else if n >= 'a'.toNat /\ n <= 'f'.toNat then some (n - 'a'.toNat + 10)
  else if n >= 'A'.toNat /\ n <= 'F'.toNat then some (n - 'A'.toNat + 10)
  else none


private def isSpace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r'


private partial def blockCommentRest : Parser Unit := do
  let c <- any
  if c == '*' then
    match (<- peek?) with
    | some '/' => discard any
    | _        => blockCommentRest
  else
    blockCommentRest


private partial def skipWs : Parser Unit := do
  discard (many (satisfy isSpace))
  (do
    (attempt (skipString "//") *> discard (many (satisfy (fun c => c != '\n'))))
      <|> (attempt (skipString "/*") *> blockCommentRest)
    skipWs) <|> pure ()


private def hexWord : Parser AddrWord := do
  skipString "0x"
  let ds <- many1 hexDigit
  let n := ds.foldl (fun acc c => acc * 16 + (hexValue? c).getD 0) 0
  if n > 0xFFFF then fail "address is outside the 16-bit address range"
  return BitVec.ofNat 16 n


private def capsToken : Parser MemCaps :=
  (do skipChar '-'; return { r := false, w := false, x := false })
    <|> (do
      let s <- many1Chars (satisfy (fun c => c == 'r' || c == 'w' || c == 'x'))
      return MemCaps.ofString s)


private def identifier : Parser String :=
  many1Chars (satisfy (fun c => c.isAlphanum || c == '_'))


private def regionBody : Parser MemRegion := do
  let lo <- hexWord
  skipWs; skipChar '-'; skipWs
  let hi <- hexWord
  skipWs; skipChar ':'; skipWs
  let c <- capsToken
  skipWs; skipChar ';'
  if hi < lo then fail "region end precedes its start"
  return { lo := lo, hi := hi, caps := c }


private def skipToSemi : Parser Unit := do
  discard (many (satisfy (fun c => c != ';')))
  skipChar ';'


private def row : Parser (Option MemRegion) := do
  let kw <- identifier
  skipWs
  let result <- match kw.toLower with
    | "memory"  => do let reg <- regionBody; pure (some reg)
    | "section" => do skipToSemi; pure none
    | "mpu"     => do skipToSemi; pure none
    | _         => fail s!"unknown statement '{kw}', expected memory, section, or mpu"
  skipWs
  return result


private def memMapFile : Parser MemMap := do
  skipWs
  let rows <- many row
  eof
  return rows.toList.filterMap id


def parseMemMap (s : String) : Except String MemMap :=
  memMapFile.run s

end Parser


end MisaSpec
