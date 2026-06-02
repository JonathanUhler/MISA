module Decoder (decodeBinaryObject) where


import Control.Monad (when)
import Data.Bits ((.&.), shiftR)
import Data.List (sortBy, partition)
import Data.Ord (comparing)
import Data.Word (Word8, Word16)
import Grammar
import ObjectFile

import Debug.Trace


decodeBinaryObject :: BinaryObject -> Bool -> Program
decodeBinaryObject secs decodeAll = concatMap decodeSec secsToDecode
  where secsToDecode = filter (\(Sec name _ _ _) -> name == "text" || decodeAll) secs


decodeSec :: Sec -> Program
decodeSec (Sec _ [LiteralCode binary] syms relocs) = decodedBytes
  where
    sortedSyms   = sortBy (comparing (\(Sym _   addr) -> addr)) syms
    sortedRelocs = sortBy (comparing (\(Reloc kind addr _) -> (addr, kind))) relocs
    decodedBytes = decodeBytes binary sortedSyms sortedRelocs
decodeSec _ = undefined


{- |
Walks a LiteralCode byte array once from address 0, consuming *sorted* symbols and relocations
as their addresses appear, and accumulates a [Stat] list that is returned at the end. The cases
handled at each step in priority order are:

- One or more symbols land exactly on the current address: emit label statements.
- A symbol lands on the next address: symbols cannot be defined in the middle of instructions, so
  emit the current byte as a .word, then the label, then restart decoding at the next byte.
- A LowReloc lands on the current address: if there is a matching HighReloc for the next byte, emit
  a .addr directive. Otherwise, the reloc is nonsensical so skip it.
- A HighReloc lands on the current address: this is always nonsensical so skip it.
- Otherwise: decode the current and next bytes as an instruction, applying any reloc that targets
  the high byte if this instruction is SET.

After the byte array is exhausted, trailing symbols become labels preceded by .space padding, and
trailing relocations are paired into .addr directives the same way as in the loop.
-}
decodeBytes :: [Word8] -> [Sym] -> [Reloc] -> Program
decodeBytes bytes syms relocs = reverse (decodeAndAcc bytes syms relocs 0 [])
  where decodeAndAcc []  syms relocs addr stats = addTrailing syms relocs addr stats
        decodeAndAcc [b] syms relocs addr stats = decodeAndAcc [] syms1 relocs (addr + 1) stats2
          where (stats1, syms1) = addCurrentSyms syms addr stats
                stats2          = DirStat (WordDir b) : stats1
        decodeAndAcc (lo : hi : rest) syms relocs addr stats =
          let
            (stats1, syms1) = addCurrentSyms syms addr stats
          in
            if hasUnalignedSyms syms addr then
              decodeAndAcc (hi : rest) syms1 relocs (addr + 1) (DirStat (WordDir lo) : stats1)
            else
              let
                (stats2,    relocs1) = addAddressRelocs relocs addr stats1
                (instStats, relocs2) = decodeInstWithReloc lo hi relocs addr
              in
                decodeAndAcc rest syms1 relocs2 (addr + 2) (prependAll instStats stats2)


addTrailing :: SymTable -> RelocTable -> Word16 -> Program -> Program
addTrailing [] [] addr stats = stats
addTrailing (sym : syms) [] addr stats
  = addTrailing syms [] newAddr newStats
  where (newStats, newAddr) = addTrailingSym sym addr stats
addTrailing [] (loReloc : hiReloc : relocs) addr stats
  = addTrailing [] relocs newAddr newStats
  where (newStats, newAddr) = addTrailingReloc loReloc hiReloc addr stats
addTrailing (sym : syms) (loReloc : hiReloc : relocs) addr stats
  | symAddr <= relocAddr = addTrailing syms relocs addr2 stats2
  where (Sym _ symAddr)       = sym
        (Reloc _ relocAddr _) = loReloc
        (stats1, addr1)       = addTrailingSym sym addr stats
        (stats2, addr2)       = addTrailingReloc loReloc hiReloc addr1 stats1
addTrailing (sym : syms) (loReloc : hiReloc : relocs) addr stats
  | symAddr > relocAddr = addTrailing syms relocs addr2 stats2
  where (Sym _ symAddr)       = sym
        (Reloc _ relocAddr _) = loReloc
        (stats1, addr1)       = addTrailingReloc loReloc hiReloc addr  stats
        (stats2, addr2)       = addTrailingSym sym addr1 stats1


addTrailingSym :: Sym -> Word16 -> Program -> (Program, Word16)
addTrailingSym (Sym label symAddr) addr stats = (newStats, symAddr)
  where spaceCount = symAddr - addr
        space      = DirStat (SpaceDir spaceCount)
        newStats   = LabelStat label : (if spaceCount > 0 then space : stats else stats)


addTrailingReloc :: Reloc -> Reloc -> Word16 -> Program -> (Program, Word16)
addTrailingReloc (Reloc LowReloc loAddr loLabel) (Reloc HighReloc hiAddr hiLabel) addr stats
  | (hiAddr == loAddr + 1 && hiLabel == loLabel) = (newStats, loAddr + 2)
  where spaceCount = loAddr - addr
        space      = DirStat (SpaceDir spaceCount)
        newStats   = DirStat (AddrDir loLabel) : (if spaceCount > 0 then space : stats else stats)


addCurrentSyms :: SymTable -> Word16 -> Program -> (Program, SymTable)
addCurrentSyms syms addr stats = (prependAll labelStats stats, symsLeft)
  where (labelStats, symsLeft) = symsAt addr syms


hasUnalignedSyms :: SymTable -> Word16 -> Bool
hasUnalignedSyms syms addr = labelStats /= []
  where (labelStats, _) = symsAt (addr + 1) syms


addAddressRelocs :: RelocTable -> Word16 -> Program -> (Program, RelocTable)
addAddressRelocs (Reloc LowReloc loAddr loLabel : Reloc HighReloc hiAddr hiLabel : rest) addr stats
  | loAddr == addr && hiAddr == loAddr + 1 && loLabel == hiLabel = (addrStat : stats, rest)
  where addrStat = DirStat (AddrDir loLabel)
addAddressRelocs relocs _ stats = (stats, relocs)


prependAll :: [Stat] -> Program -> Program
prependAll stats prog = foldl (flip (:)) prog stats


{- |
Gets all symbols with the provided address from a list of symbols.

The returned value is a pair of lists. The first list contains LabelStats with the label names
defined in the symbols. The second list is the symbols with the specified address.
-}
symsAt :: Word16 -> SymTable -> ([Stat], SymTable)
symsAt addr syms = (symLabels, symsLeft)
  where (symsAtAddr, symsLeft) = partition (\(Sym _ a) -> a == addr) syms
        symLabels              = map (\(Sym l _) -> LabelStat l) symsAtAddr


{- | Gets the address field of a Sym. -}
symAddr :: Sym -> Word16
symAddr (Sym _ a) = a


-- | Decode two bytes as an instruction, attaching a label immediate when a
-- relocation targets the hi byte.  SET instructions receive the reloc's label
-- and kind; relocs on other instructions or on undecoded bytes are consumed
-- with a warning.  Always returns exactly one Stat on a successful decode and
-- two .word DirStats on failure.
decodeInstWithReloc :: Word8 -> Word8 -> [Reloc] -> Word16 -> ([Stat], [Reloc])
decodeInstWithReloc lo hi relocs addr =
  case (decodeInst lo hi, hiReloc) of
    (Just (SetInst rd _), Just (Reloc kind _ label)) -> ([InstStat setInst], newRelocs)
      where setInst = SetInst rd (LabelImm immPart label)
            immPart = if kind == HighReloc then High else Low
    (Just inst,           Just _)  -> ([InstStat inst], newRelocs)
    (Just inst,           Nothing) -> ([InstStat inst], newRelocs)
    (Nothing,             _)       -> ([DirStat (WordDir lo), DirStat (WordDir hi)], newRelocs)
  where (hiReloc, newRelocs) = case relocs of
          (r@(Reloc _ hiAddr _) : rs) | hiAddr == addr + 1 -> (Just r,  rs)
          _                                                -> (Nothing, relocs)

{- |
Attempts to decode the two-byte little-endian sequence {lo, hi} as a core instruction, with support
for all architectural extensions.

Decoding will be based on the least-significant nibble (the opcode), which then dictates which
operands are required. Nothing will only be returned if the two bytes represent an illegal encoding
(e.g. a CSR or jump code that doesn't exist).
-}
decodeInst :: Word8 -> Word8 -> Maybe Inst
decodeInst lo hi = inst
  where
    nib0  = lo .&. 0xF
    nib1  = fromIntegral (lo `shiftR` 4)
    nib2  = fromIntegral (hi .&. 0xF)
    nib3  = fromIntegral (hi `shiftR` 4)
    word1 = fromIntegral hi
    inst  = case nib0 of
      0x0 | nib2 == 0 && nib3 == 8 -> Just (SyscallInst (toEnum nib1))
      0x0 | nib2 == 0 && nib3 == 0 -> Just (HaltInst    (toEnum nib1))
      0x1                           -> Just (AddInst (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0x2                           -> Just (AdcInst (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0x3                           -> Just (SubInst (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0x4                           -> Just (SbbInst (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0x5                           -> Just (AndInst (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0x6                           -> Just (OrInst  (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0x7                           -> Just (XorInst (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0x8 | nib3 == 0               -> Just (RrcInst (toEnum nib1) (toEnum nib2))
      0x9                           -> Just (SetInst (toEnum nib1) (IntImm Full word1))
      0xA                           -> Just (LdInst  (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0xB                           -> Just (StInst  (toEnum nib1) (toEnum nib2) (toEnum nib3))
      0xC | Just csr <- toCsr nib3  -> Just (RsrInst csr (toEnum nib1) (toEnum nib2))
      0xD | Just csr <- toCsr nib3  -> Just (WsrInst csr (toEnum nib1) (toEnum nib2))
      0xE | Just cmp <- toCmp nib3  -> Just (JalInst cmp (toEnum nib1) (toEnum nib2))
      0xF | Just cmp <- toCmp nib3  -> Just (JmpInst cmp (toEnum nib1) (toEnum nib2))
      _                             -> Nothing
    toCsr nib = case nib of
      0x1 -> Just SADDR
      0x2 -> Just RADDR
      0x3 -> Just FLAGS
      0x4 -> Just CAUSE
      0x5 -> Just EXTNS
      0x8 -> Just RETSC
      0xA -> Just PRIVS
      _   -> Nothing
    toCmp nib = case nib of
      0x0 -> Just ALWAYS
      0x1 -> Just EQUAL
      0x8 -> Just NOT_EQUAL
      0x2 -> Just GREATER
      0x4 -> Just LESS
      0x3 -> Just GREATER_EQUAL
      0x5 -> Just LESS_EQUAL
      _   -> Nothing
