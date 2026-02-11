module EncoderSpec (spec) where



import Encoder
import Grammar
import Lexer
import ObjectFile
import Parser

import Test.Hspec


spec :: Spec
spec = do
  describe "encoderRun" $ do
    it "encodes the empty program" $
      encoderRun [] `shouldBe` []
    it "encodes one instruction with no sections" $
      encoderRun (fst $ parserRun (fst $ lexerRun "ADD R1 R2 R3")) `shouldBe`
      [
        Section "text"
        [InstructionCode (AddInstruction R1 R2 R3)]
        []
        []
      ]
    it "encodes one label with no sections" $
      encoderRun (fst $ parserRun (fst $ lexerRun "LOOP:")) `shouldBe`
      [
        Section "text"
        []
        [Symbol "LOOP" 0]
        []
      ]
    it "encodes one directive with no sections" $ do
      encoderRun (fst $ parserRun (fst $ lexerRun ".word 1")) `shouldBe`
        [
          Section "text"
          [LiteralCode [1]]
          []
          []
        ]
      encoderRun (fst $ parserRun (fst $ lexerRun ".array 1 2 3")) `shouldBe`
        [
          Section "text"
          [LiteralCode [1, 2, 3]]
          []
          []
        ]
    it "encodes many instructions with no sections" $
      encoderRun (fst $ parserRun (fst $ lexerRun "LI R1 0 LI R2 1 ADD R3 R1 R2")) `shouldBe`
      [
        Section "text"
        [InstructionCode (LiInstruction R1 (IntImmediate 0)),
         InstructionCode (LiInstruction R2 (IntImmediate 1)),
         InstructionCode (AddInstruction R3 R1 R2)]
        []
        []
      ]
    it "encodes many labels with no sections" $
      encoderRun (fst $ parserRun (fst $ lexerRun "LOOP: END_LOOP:")) `shouldBe`
      [
        Section "text"
        []
        [Symbol "LOOP" 0, Symbol "END_LOOP" 0]
        []
      ]
    it "encodes many directives with no sections" $ do
      encoderRun (fst $ parserRun (fst $ lexerRun ".word 1 .word 2 .array 3 4 5")) `shouldBe`
        [
          Section "text"
          [LiteralCode [1], LiteralCode [2], LiteralCode [3, 4, 5]]
          []
          []
        ]
    it "encodes many statements with no sections" $
      encoderRun (fst $ parserRun (fst $ lexerRun "LARGEST: .word 233 .word 0 LI R1 0 LI R2 1 LOOP: ADD R3 R1 R2 OR R1 R2 R0 OR R2 R3 R0 JZ R0 LOOP")) `shouldBe`
      [
        Section "text"
        [LiteralCode [233],
         LiteralCode [0],
         InstructionCode (LiInstruction R1 (IntImmediate 0)),
         InstructionCode (LiInstruction R2 (IntImmediate 1)),
         InstructionCode (AddInstruction R3 R1 R2),
         InstructionCode (OrInstruction R1 R2 R0),
         InstructionCode (OrInstruction R2 R3 R0),
         InstructionCode (JzInstruction R0 (LabelImmediate "LOOP"))]
        [Symbol "LARGEST" 0, Symbol "LOOP" 6]
        [Relocation LowRelocation 13 "LOOP"]
      ]
    it "encodes many statements with no initial section" $
      encoderRun (fst $ parserRun (fst $ lexerRun "LARGEST: .word 233 .word 0 .section code LI R1 0 LI R2 1 LOOP: ADD R3 R1 R2 OR R1 R2 R0 OR R2 R3 R0 JZ R0 LOOP")) `shouldBe`
      [
        Section "code"
        [InstructionCode (LiInstruction R1 (IntImmediate 0)),
         InstructionCode (LiInstruction R2 (IntImmediate 1)),
         InstructionCode (AddInstruction R3 R1 R2),
         InstructionCode (OrInstruction R1 R2 R0),
         InstructionCode (OrInstruction R2 R3 R0),
         InstructionCode (JzInstruction R0 (LabelImmediate "LOOP"))]
        [Symbol "LOOP" 4]
        [Relocation LowRelocation 11 "LOOP"],

        Section "text"
        [LiteralCode [233], LiteralCode [0]]
        [Symbol "LARGEST" 0]
        []
      ]
    it "encodes many statements with one initial section" $
      encoderRun (fst $ parserRun (fst $ lexerRun ".section code LARGEST: .word 233 .word 0 LI R1 0 LI R2 1 LOOP: ADD R3 R1 R2 OR R1 R2 R0 OR R2 R3 R0 JZ R0 LOOP")) `shouldBe`
      [
        Section "code"
        [LiteralCode [233],
         LiteralCode [0],
         InstructionCode (LiInstruction R1 (IntImmediate 0)),
         InstructionCode (LiInstruction R2 (IntImmediate 1)),
         InstructionCode (AddInstruction R3 R1 R2),
         InstructionCode (OrInstruction R1 R2 R0),
         InstructionCode (OrInstruction R2 R3 R0),
         InstructionCode (JzInstruction R0 (LabelImmediate "LOOP"))]
        [Symbol "LARGEST" 0, Symbol "LOOP" 6]
        [Relocation LowRelocation 13 "LOOP"]
      ]
    it "encodes many statements with many sections" $
      encoderRun (fst $ parserRun (fst $ lexerRun ".section data LARGEST: .word 233 .word 0 .section code LI R1 0 LI R2 1 LOOP: ADD R3 R1 R2 OR R1 R2 R0 OR R2 R3 R0 JZ R0 LOOP")) `shouldBe`
      [
        Section "code"
        [InstructionCode (LiInstruction R1 (IntImmediate 0)),
         InstructionCode (LiInstruction R2 (IntImmediate 1)),
         InstructionCode (AddInstruction R3 R1 R2),
         InstructionCode (OrInstruction R1 R2 R0),
         InstructionCode (OrInstruction R2 R3 R0),
         InstructionCode (JzInstruction R0 (LabelImmediate "LOOP"))]
        [Symbol "LOOP" 4]
        [Relocation LowRelocation 11 "LOOP"],

        Section "data"
        [LiteralCode [233], LiteralCode [0]]
        [Symbol "LARGEST" 0]
        []
      ]
    it "encodes many statements with duplicate sections" $
      encoderRun (fst $ parserRun (fst $ lexerRun ".section data .word 1 .section code LI R1 0 .section data .array 2 3 4 5")) `shouldBe`
      [
        Section "code"
        [InstructionCode (LiInstruction R1 (IntImmediate 0))]
        []
        [],

        Section "data"
        [LiteralCode [1], LiteralCode [2, 3, 4, 5]]
        []
        []
      ]
