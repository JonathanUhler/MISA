module Names (formatNames) where


import ObjectFile

import Text.Printf (printf)


formatSyms :: SymTable -> String
formatSyms []           = ""
formatSyms (sym : syms) = formatSym sym ++ formatSyms syms


formatSym :: Sym -> String
formatSym (Sym name addr) = "<- " ++ (printf "%04X" addr) ++ " " ++ name


formatRelocs :: RelocTable -> String
formatRelocs []               = ""
formatRelocs (reloc : relocs) = formatReloc reloc ++ formatRelocs relocs


formatReloc :: Reloc -> String
formatReloc (Reloc kind addr name) = "-> " ++ (printf "%04X" addr) ++ " " ++ name ++ " " ++ kindStr
  where kindStr = case kind of
                    LowReloc  -> "(lo)"
                    HighReloc -> "(hi)"


formatSecs :: BinaryObject -> String
formatSecs []           = ""
formatSecs (sec : secs) = formatSec sec ++ formatSecs secs


formatSec :: Sec -> String
formatSec (Sec name _ syms relocs) =
  "Section " ++ name ++ ":\n\n" ++ formatSyms syms ++ "\n" ++ formatRelocs relocs


formatNames :: BinaryObject -> String
formatNames []  = "No sections"
formatNames obj = formatSecs obj
