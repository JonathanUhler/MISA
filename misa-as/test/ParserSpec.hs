module ParserSpec (spec) where


import Grammar
import Lexer
import Parser

import Test.Hspec


spec :: Spec
spec = do
  describe "parserRun" $ do
    it "parses nothing" $
      parserRun [] `shouldBe` ([], [])
    it "parses single instructions" $ do
      parserRun (fst (lexerRun "ADD R1 R2 R3")) `shouldBe`
        ([InstructionStatement (AddInstruction R1 R2 R3)], [])
      parserRun (fst (lexerRun "ADC R1 R2 R3")) `shouldBe`
        ([InstructionStatement (AdcInstruction R1 R2 R3)], [])
      parserRun (fst (lexerRun "SUB R1 R2 R3")) `shouldBe`
        ([InstructionStatement (SubInstruction R1 R2 R3)], [])
      parserRun (fst (lexerRun "AND R1 R2 R3")) `shouldBe`
        ([InstructionStatement (AndInstruction R1 R2 R3)], [])
      parserRun (fst (lexerRun "OR R1 R2 R3")) `shouldBe`
        ([InstructionStatement (OrInstruction R1 R2 R3)], [])
      parserRun (fst (lexerRun "XOR R1 R2 R3")) `shouldBe`
        ([InstructionStatement (XorInstruction R1 R2 R3)], [])
      parserRun (fst (lexerRun "LW R1 0")) `shouldBe`
        ([InstructionStatement (LwInstruction R1 0)], [])
      parserRun (fst (lexerRun "SW R1 0")) `shouldBe`
        ([InstructionStatement (SwInstruction R1 0)], [])
      parserRun (fst (lexerRun "LA R1 R2")) `shouldBe`
        ([InstructionStatement (LaInstruction R1 R2)], [])
      parserRun (fst (lexerRun "SA R1 R2")) `shouldBe`
        ([InstructionStatement (SaInstruction R1 R2)], [])
      parserRun (fst (lexerRun "LI R1 0")) `shouldBe`
        ([InstructionStatement (LiInstruction R1 0)], [])
      parserRun (fst (lexerRun "JLZ R1 0")) `shouldBe`
        ([InstructionStatement (JlzInstruction R1 0)], [])
      parserRun (fst (lexerRun "HALT 0")) `shouldBe`
        ([InstructionStatement (HaltInstruction 0)], [])
    it "parses single labels" $ do
      parserRun (fst (lexerRun "_:")) `shouldBe` ([LabelStatement "_"], [])
      parserRun (fst (lexerRun "x:")) `shouldBe` ([LabelStatement "x"], [])
      parserRun (fst (lexerRun "X:")) `shouldBe` ([LabelStatement "X"], [])
      parserRun (fst (lexerRun "xs:")) `shouldBe` ([LabelStatement "xs"], [])
      parserRun (fst (lexerRun "Xs:")) `shouldBe` ([LabelStatement "Xs"], [])
      parserRun (fst (lexerRun "_xs:")) `shouldBe` ([LabelStatement "_xs"], [])
      parserRun (fst (lexerRun "__x_s:")) `shouldBe` ([LabelStatement "__x_s"], [])
      parserRun (fst (lexerRun "x0:")) `shouldBe` ([LabelStatement "x0"], [])
      parserRun (fst (lexerRun "_0:")) `shouldBe` ([LabelStatement "_0"], [])
      parserRun (fst (lexerRun "_Jdwioo8__282:")) `shouldBe` ([LabelStatement "_Jdwioo8__282"], [])
    it "parses single directives" $ do
      parserRun (fst (lexerRun ".word 0")) `shouldBe`
        ([DirectiveStatement (WordDirective 0)], [])
      parserRun (fst (lexerRun ".array 0")) `shouldBe`
        ([DirectiveStatement (ArrayDirective [0])], [])
      parserRun (fst (lexerRun ".array 1 2 3 4 5 6 7 8")) `shouldBe`
        ([DirectiveStatement (ArrayDirective [1, 2, 3, 4, 5, 6, 7, 8])], [])
      parserRun (fst (lexerRun ".section foo")) `shouldBe`
        ([DirectiveStatement (SectionDirective "foo")], [])
    it "parses full programs" $ do
      parserRun (fst (lexerRun ".word 1\nLI R1 0xFF\nLI R2 1\nLOOP:\nSUB R1 R1 R2\nJLZ R1 0\n"))
        `shouldBe`
        ([DirectiveStatement (WordDirective 1),
          InstructionStatement (LiInstruction R1 255),
          InstructionStatement (LiInstruction R2 1),
          LabelStatement "LOOP",
          InstructionStatement (SubInstruction R1 R1 R2),
          InstructionStatement (JlzInstruction R1 0)], [])
    it "fails for large immediates" $ do
      parserRun (fst (lexerRun "LI R1 0x100")) `shouldBe`
        ([], fst (lexerRun "LI R1 0x100"))
      parserRun (fst (lexerRun ".word 0o400")) `shouldBe`
        ([], fst (lexerRun ".word 0o400"))
      parserRun (fst (lexerRun ".array 0b111111111")) `shouldBe`
        ([], fst (lexerRun ".array 0b111111111"))
