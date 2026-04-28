module EncoderSpec (spec) where


import Encoder
import Grammar
import ObjectFile

import Test.Hspec


spec :: Spec
spec = do
  describe "NOP resolution" $ do
    it "resolves NOP to OR R0 R0 R0" $
      resolvePseudoInst NopInst `shouldBe` [OrInst R0 R0 R0]

  describe "MOV resolution" $ do
    it "resolves MOV rd rs to OR rd rs R0" $
      resolvePseudoInst (MovInst RA RB) `shouldBe` [OrInst RA RB R0]
    it "resolves MOV with same register" $
      resolvePseudoInst (MovInst R0 R0) `shouldBe` [OrInst R0 R0 R0]
    it "resolves MOV with different registers" $
      resolvePseudoInst (MovInst RX RY) `shouldBe` [OrInst RX RY R0]

  describe "CMP resolution" $ do
    it "resolves CMP rs1 rs2 to SUB R0 rs1 rs2" $
      resolvePseudoInst (CmpInst RA RB) `shouldBe` [SubInst R0 RA RB]
    it "resolves CMP with same registers" $
      resolvePseudoInst (CmpInst RC RC) `shouldBe` [SubInst R0 RC RC]

  describe "SET2 resolution" $ do
    it "resolves SET2 rs1 rs2 imm to two SETs with high and low parts" $ do
      let result = resolvePseudoInst (Set2Inst RA RB (IntImm Full 0x1234))
      length result `shouldBe` 2
      result !! 0 `shouldBe` SetInst RA (IntImm High 0x1234)
      result !! 1 `shouldBe` SetInst RB (IntImm Low 0x1234)
    it "resolves SET2 with label immediate" $ do
      let result = resolvePseudoInst (Set2Inst RC RD (LabelImm Full "start"))
      length result `shouldBe` 2
      result !! 0 `shouldBe` SetInst RC (LabelImm High "start")
      result !! 1 `shouldBe` SetInst RD (LabelImm Low "start")

  describe "CALL resolution" $ do
    it "resolves CALL rs1 rs2 to JAL ALWAYS rs1 rs2" $
      resolvePseudoInst (CallInst RA RB) `shouldBe` [JalInst ALWAYS RA RB]
    it "resolves CALL with same registers" $
      resolvePseudoInst (CallInst RT RT) `shouldBe` [JalInst ALWAYS RT RT]

  describe "RET resolution" $ do
    it "resolves RET to RSR RADDR RY RZ followed by JMP ALWAYS RY RZ" $ do
      let result = resolvePseudoInst RetInst
      length result `shouldBe` 2
      result !! 0 `shouldBe` RsrInst RADDR RY RZ
      result !! 1 `shouldBe` JmpInst ALWAYS RY RZ

  describe "CLR resolution" $ do
    it "resolves CLR to WSR FLAGS R0 R0" $
      resolvePseudoInst ClrInst `shouldBe` [WsrInst FLAGS R0 R0]

  describe "Base instructions" $ do
    it "does not expand ADD instruction" $
      resolvePseudoInst (AddInst R0 RA RB) `shouldBe` [AddInst R0 RA RB]
    it "does not expand SUB instruction" $
      resolvePseudoInst (SubInst R0 RA RB) `shouldBe` [SubInst R0 RA RB]
    it "does not expand SET instruction" $
      resolvePseudoInst (SetInst RA (IntImm Full 100)) `shouldBe` [SetInst RA (IntImm Full 100)]
    it "does not expand JMP instruction" $
      resolvePseudoInst (JmpInst ALWAYS RA RB) `shouldBe` [JmpInst ALWAYS RA RB]

  describe "Complex resolutions" $ do
    it "resolves all pseudo instructions in a program" $ do
      let program = [InstStatement NopInst, InstStatement (MovInst RA RB)]
      let resolved = resolvePseudoInsts program
      resolved `shouldBe` [InstStatement (OrInst R0 R0 R0), InstStatement (OrInst RA RB R0)]
    it "preserves labels and directives" $ do
      let program = 
            [ LabelStatement "start"
            , InstStatement NopInst
            , DirStatement (SectionDir "text")
            , InstStatement (AddInst R0 RA RB)
            ]
      let resolved = resolvePseudoInsts program
      length resolved `shouldBe` 4
      resolved !! 0 `shouldBe` LabelStatement "start"
      resolved !! 1 `shouldBe` InstStatement (OrInst R0 R0 R0)
      resolved !! 2 `shouldBe` DirStatement (SectionDir "text")
      resolved !! 3 `shouldBe` InstStatement (AddInst R0 RA RB)
    it "handles mixed pseudo and base instructions" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , InstStatement NopInst
            , InstStatement (CmpInst RA RB)
            , InstStatement (SubInst R0 RC RD)
            ]
      let resolved = resolvePseudoInsts program
      length resolved `shouldBe` 4
      resolved !! 0 `shouldBe` InstStatement (AddInst R0 RA RB)
      resolved !! 1 `shouldBe` InstStatement (OrInst R0 R0 R0)
      resolved !! 2 `shouldBe` InstStatement (SubInst R0 RA RB)
      resolved !! 3 `shouldBe` InstStatement (SubInst R0 RC RD)
    it "handles SET2 expansion in program" $ do
      let program = [InstStatement (Set2Inst RA RB (IntImm Full 0x5678))]
      let resolved = resolvePseudoInsts program
      length resolved `shouldBe` 2
      resolved !! 0 `shouldBe` InstStatement (SetInst RA (IntImm High 0x5678))
      resolved !! 1 `shouldBe` InstStatement (SetInst RB (IntImm Low 0x5678))

  describe "extractCode" $ do
    it "extracts instructions as InstCode" $ do
      let program = [InstStatement (AddInst R0 RA RB)]
      let code = extractCode program
      code `shouldBe` [InstCode (AddInst R0 RA RB)]
    it "extracts .word directive as LiteralCode" $ do
      let program = [DirStatement (WordDir 0x42)]
      let code = extractCode program
      code `shouldBe` [LiteralCode [0x42]]
    it "extracts .array directive as LiteralCode" $ do
      let program = [DirStatement (ArrayDir [0x12, 0x34, 0x56])]
      let code = extractCode program
      code `shouldBe` [LiteralCode [0x12, 0x34, 0x56]]
    it "skips labels and section directives" $ do
      let program = 
            [ LabelStatement "start"
            , DirStatement (SectionDir "text")
            , InstStatement (AddInst R0 RA RB)
            , LabelStatement "end"
            ]
      let code = extractCode program
      code `shouldBe` [InstCode (AddInst R0 RA RB)]
    it "extracts mixed code elements" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , DirStatement (WordDir 0xFF)
            , InstStatement (SubInst R0 RC RD)
            , DirStatement (ArrayDir [0x01, 0x02])
            ]
      let code = extractCode program
      code `shouldBe` 
        [ InstCode (AddInst R0 RA RB)
        , LiteralCode [0xFF]
        , InstCode (SubInst R0 RC RD)
        , LiteralCode [0x01, 0x02]
        ]

  describe "extractSyms" $ do
    it "extracts label at address 0" $ do
      let program = [LabelStatement "start"]
      let syms = extractSyms program
      syms `shouldBe` [Sym "start" 0]
    it "calculates correct address after instruction" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , LabelStatement "next"
            ]
      let syms = extractSyms program
      syms `shouldBe` [Sym "next" 2]
    it "calculates correct address after .word directive" $ do
      let program = 
            [ DirStatement (WordDir 0x42)
            , LabelStatement "after_word"
            ]
      let syms = extractSyms program
      syms `shouldBe` [Sym "after_word" 1]
    it "calculates correct address after .array directive" $ do
      let program = 
            [ DirStatement (ArrayDir [0x12, 0x34, 0x56, 0x78])
            , LabelStatement "after_array"
            ]
      let syms = extractSyms program
      syms `shouldBe` [Sym "after_array" 4]
    it "extracts multiple labels with correct addresses" $ do
      let program = 
            [ LabelStatement "start"
            , InstStatement (AddInst R0 RA RB)
            , InstStatement (SubInst R0 RC RD)
            , LabelStatement "middle"
            , DirStatement (WordDir 0xFF)
            , LabelStatement "end"
            ]
      let syms = extractSyms program
      syms `shouldBe` 
        [ Sym "start" 0
        , Sym "middle" 4
        , Sym "end" 5
        ]
    it "skips section directives in address calculation" $ do
      let program = 
            [ DirStatement (SectionDir "text")
            , LabelStatement "start"
            ]
      let syms = extractSyms program
      syms `shouldBe` [Sym "start" 0]

  describe "extractRelocs" $ do
    it "finds relocation for SET instruction with label" $ do
      let program = [InstStatement (SetInst RA (LabelImm Full "symbol"))]
      let relocs = extractRelocs program
      length relocs `shouldBe` 1
      let (Reloc typ addr label) = head relocs
      typ `shouldBe` LowReloc
      addr `shouldBe` 1
      label `shouldBe` "symbol"
    it "finds relocation for SET instruction with full part" $ do
      let program = [InstStatement (SetInst RA (LabelImm Full "symbol"))]
      let relocs = extractRelocs program
      let (Reloc typ _ _) = head relocs
      typ `shouldBe` LowReloc
    it "finds relocation for SET instruction with low part" $ do
      let program = [InstStatement (SetInst RA (LabelImm Low "symbol"))]
      let relocs = extractRelocs program
      let (Reloc typ _ _) = head relocs
      typ `shouldBe` LowReloc
    it "finds relocation for SET instruction with high part" $ do
      let program = [InstStatement (SetInst RA (LabelImm High "symbol"))]
      let relocs = extractRelocs program
      let (Reloc typ _ _) = head relocs
      typ `shouldBe` HighReloc
    it "does not create relocations for integer immediates" $ do
      let program = [InstStatement (SetInst RA (IntImm Full 100))]
      let relocs = extractRelocs program
      relocs `shouldBe` []
    it "calculates correct relocation addresses" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , InstStatement (SetInst RC (LabelImm Full "loop"))
            , InstStatement (SubInst R0 RD RE)
            ]
      let relocs = extractRelocs program
      length relocs `shouldBe` 1
      let (Reloc _ addr _) = head relocs
      addr `shouldBe` 3
    it "finds multiple relocations" $ do
      let program = 
            [ InstStatement (SetInst RA (LabelImm Full "sym1"))
            , InstStatement (SetInst RB (LabelImm Full "sym2"))
            ]
      let relocs = extractRelocs program
      length relocs `shouldBe` 2
      let addrs = map (\(Reloc _ addr _) -> addr) relocs
      addrs `shouldBe` [1, 3]
    it "finds relocations after .word directives" $ do
      let program = 
            [ DirStatement (WordDir 0xFF)
            , InstStatement (SetInst RA (LabelImm Full "symbol"))
            ]
      let relocs = extractRelocs program
      length relocs `shouldBe` 1
      let (Reloc _ addr _) = head relocs
      addr `shouldBe` 2
    it "skips non-label instructions" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , InstStatement (JmpInst ALWAYS RA RB)
            ]
      let relocs = extractRelocs program
      relocs `shouldBe` []

  describe "splitSecs" $ do
    it "returns single text section for program without section directives" $ do
      let program = [InstStatement (AddInst R0 RA RB)]
      let secs = splitSecs program
      length secs `shouldBe` 1
      let (secName, _) = head secs
      secName `shouldBe` "text"
    it "splits program by section directives" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , DirStatement (SectionDir "data")
            , DirStatement (WordDir 0x42)
            ]
      let secs = splitSecs program
      length secs `shouldBe` 2
      let secNames = map fst secs
      secNames `shouldBe` ["data", "text"]
    it "groups statements with same section together" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , DirStatement (SectionDir "data")
            , DirStatement (WordDir 0x42)
            , DirStatement (WordDir 0x43)
            , DirStatement (SectionDir "text")
            , InstStatement (SubInst R0 RC RD)
            ]
      let secs = splitSecs program
      length secs `shouldBe` 2
      let secNames = map fst secs
      secNames `shouldBe` ["data", "text"]
    it "returns statements in correct section" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , DirStatement (SectionDir "data")
            , DirStatement (WordDir 0x42)
            ]
      let secs = splitSecs program
      let (_, textStmts) = secs !! 1
      let (_, dataStmts) = head secs
      length textStmts `shouldBe` 1
      length dataStmts `shouldBe` 1
    it "handles multiple section directives in sequence" $ do
      let program = 
            [ DirStatement (SectionDir "text")
            , InstStatement (AddInst R0 RA RB)
            , DirStatement (SectionDir "data")
            , DirStatement (SectionDir "rodata")
            , DirStatement (WordDir 0x42)
            ]
      let secs = splitSecs program
      length secs `shouldBe` 3

  describe "encoderCreateSec" $ do
    it "creates section with code from instructions" $ do
      let program = [InstStatement (AddInst R0 RA RB)]
      let sec = encoderCreateSec ("text", program)
      case sec of
        Sec name code _ _ -> do
          name `shouldBe` "text"
          length code `shouldBe` 1
    it "creates section with symbols from labels" $ do
      let program = [LabelStatement "start"]
      let sec = encoderCreateSec ("text", program)
      case sec of
        Sec _ _ syms _ -> do
          length syms `shouldBe` 1
          let (Sym label _) = head syms
          label `shouldBe` "start"
    it "creates section with relocations from label immediates" $ do
      let program = [InstStatement (SetInst RA (LabelImm Full "sym"))]
      let sec = encoderCreateSec ("text", program)
      case sec of
        Sec _ _ _ relocs -> do
          length relocs `shouldBe` 1
    it "creates complete section from mixed program" $ do
      let program = 
            [ LabelStatement "start"
            , InstStatement (AddInst R0 RA RB)
            , InstStatement (SetInst RC (LabelImm Full "loop"))
            ]
      let sec = encoderCreateSec ("text", program)
      case sec of
        Sec name code syms relocs -> do
          name `shouldBe` "text"
          length code `shouldBe` 2
          length syms `shouldBe` 1
          length relocs `shouldBe` 1

  describe "encodeProgram" $ do
    it "encodes empty program as single empty text section" $ do
      let program = []
      let obj = encodeProgram program
      length obj `shouldBe` 0
    it "encodes program with single instruction" $ do
      let program = [InstStatement (AddInst R0 RA RB)]
      let obj = encodeProgram program
      length obj `shouldBe` 1
    it "encodes program with pseudo instructions (expands them)" $ do
      let program = [InstStatement NopInst, InstStatement (MovInst RA RB)]
      let obj = encodeProgram program
      length obj `shouldBe` 1
      let (Sec _ code _ _) = head obj
      length code `shouldBe` 2
    it "encodes program with labels and instructions" $ do
      let program = 
            [ LabelStatement "start"
            , InstStatement (AddInst R0 RA RB)
            ]
      let obj = encodeProgram program
      let (Sec _ code syms _) = head obj
      length code `shouldBe` 1
      length syms `shouldBe` 1
    it "encodes program with multiple sections" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , DirStatement (SectionDir "data")
            , DirStatement (WordDir 0x42)
            ]
      let obj = encodeProgram program
      length obj `shouldBe` 2
    it "encodes program with directives and labels" $ do
      let program = 
            [ LabelStatement "data_start"
            , DirStatement (WordDir 0x42)
            , LabelStatement "data_end"
            ]
      let obj = encodeProgram program
      let (Sec _ code syms _) = head obj
      length code `shouldBe` 1
      length syms `shouldBe` 2
    it "encodes program with .array directive" $ do
      let program = [DirStatement (ArrayDir [0x11, 0x22, 0x33, 0x44])]
      let obj = encodeProgram program
      let (Sec _ code _ _) = head obj
      length code `shouldBe` 1
    it "encodes complex program with all features" $ do
      let program = 
            [ LabelStatement "start"
            , InstStatement (AddInst R0 RA RB)
            , InstStatement NopInst
            , InstStatement (SetInst RC (LabelImm Full "loop"))
            , DirStatement (WordDir 0xFF)
            , LabelStatement "loop"
            , InstStatement (SubInst R0 RC RD)
            ]
      let obj = encodeProgram program
      length obj `shouldBe` 1
      let (Sec name code syms relocs) = head obj
      name `shouldBe` "text"
      length code `shouldBe` 5
      length syms `shouldBe` 2
      length relocs `shouldBe` 1
    it "preserves section names in encoding" $ do
      let program = 
            [ InstStatement (AddInst R0 RA RB)
            , DirStatement (SectionDir "rodata")
            , DirStatement (WordDir 0x42)
            , DirStatement (SectionDir "data")
            , DirStatement (WordDir 0x43)
            ]
      let obj = encodeProgram program
      let names = map (\(Sec n _ _ _) -> n) obj
      names `shouldBe` ["data", "rodata", "text"]
