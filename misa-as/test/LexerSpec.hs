module LexerSpec (spec) where


import Grammar
import Lexer

import Test.Hspec


spec :: Spec
spec = do
  describe "lexerRun" $ do
    it "lexes nothing" $
      lexerRun "" `shouldBe` ([], "")
    it "lexes whitespace" $
      lexerRun "\n\r\t " `shouldBe` ([], "")
    it "lexes comments" $
      lexerRun "; comment" `shouldBe` ([], "")
    it "lexes nested comments" $
      lexerRun "; comment ; another semicolon" `shouldBe` ([], "")
    it "lexes colon" $
      lexerRun ":" `shouldBe` ([ColonToken], "")
    it "lexes period" $
      lexerRun "." `shouldBe` ([PeriodToken], "")
    it "lexes opcodes" $ do
      lexerRun "ADD" `shouldBe` ([OpcodeToken ADD], "")
      lexerRun "ADC" `shouldBe` ([OpcodeToken ADC], "")
      lexerRun "SUB" `shouldBe` ([OpcodeToken SUB], "")
      lexerRun "AND" `shouldBe` ([OpcodeToken AND], "")
      lexerRun "OR" `shouldBe` ([OpcodeToken OR], "")
      lexerRun "XOR" `shouldBe` ([OpcodeToken XOR], "")
      lexerRun "LW" `shouldBe` ([OpcodeToken LW], "")
      lexerRun "SW" `shouldBe` ([OpcodeToken SW], "")
      lexerRun "LA" `shouldBe` ([OpcodeToken LA], "")
      lexerRun "SA" `shouldBe` ([OpcodeToken SA], "")
      lexerRun "LI" `shouldBe` ([OpcodeToken LI], "")
      lexerRun "JLZ" `shouldBe` ([OpcodeToken JLZ], "")
      lexerRun "HALT" `shouldBe` ([OpcodeToken HALT], "")
    it "lexes registers" $ do
      lexerRun "R0" `shouldBe` ([RegisterToken R0], "")
      lexerRun "R1" `shouldBe` ([RegisterToken R1], "")
      lexerRun "R2" `shouldBe` ([RegisterToken R2], "")
      lexerRun "R3" `shouldBe` ([RegisterToken R3], "")
      lexerRun "R4" `shouldBe` ([RegisterToken R4], "")
      lexerRun "R5" `shouldBe` ([RegisterToken R5], "")
      lexerRun "R6" `shouldBe` ([RegisterToken R6], "")
      lexerRun "R7" `shouldBe` ([RegisterToken R7], "")
      lexerRun "R8" `shouldBe` ([RegisterToken R8], "")
      lexerRun "R9" `shouldBe` ([RegisterToken R9], "")
      lexerRun "R10" `shouldBe` ([RegisterToken R10], "")
      lexerRun "R11" `shouldBe` ([RegisterToken R11], "")
      lexerRun "R12" `shouldBe` ([RegisterToken R12], "")
      lexerRun "R13" `shouldBe` ([RegisterToken R13], "")
      lexerRun "R14" `shouldBe` ([RegisterToken R14], "")
      lexerRun "R15" `shouldBe` ([RegisterToken R15], "")
    it "lexes single-digit numbers" $ do
      lexerRun "0x1" `shouldBe` ([NumberToken 1], "")
      lexerRun "0b1" `shouldBe` ([NumberToken 1], "")
      lexerRun "0o1" `shouldBe` ([NumberToken 1], "")
      lexerRun "1" `shouldBe` ([NumberToken 1], "")
    it "lexes multi-digit numbers" $ do
      lexerRun "0x10" `shouldBe` ([NumberToken 16], "")
      lexerRun "0b10" `shouldBe` ([NumberToken 2], "")
      lexerRun "0o10" `shouldBe` ([NumberToken 8], "")
      lexerRun "10" `shouldBe` ([NumberToken 10], "")
      lexerRun "-10" `shouldBe` ([], "-10")
    it "lexes large numbers" $ do
      lexerRun "0xFFFFFFFF" `shouldBe` ([NumberToken 4294967295], "")
      lexerRun "0o77777777" `shouldBe` ([NumberToken 16777215], "")
      lexerRun "0b11111111" `shouldBe` ([NumberToken 255], "")
      lexerRun "99999999" `shouldBe` ([NumberToken 99999999], "")
    it "lexes identifiers" $ do
      lexerRun "_" `shouldBe` ([IdentifierToken "_"], "")
      lexerRun "x" `shouldBe` ([IdentifierToken "x"], "")
      lexerRun "X" `shouldBe` ([IdentifierToken "X"], "")
      lexerRun "xs" `shouldBe` ([IdentifierToken "xs"], "")
      lexerRun "Xs" `shouldBe` ([IdentifierToken "Xs"], "")
      lexerRun "_xs" `shouldBe` ([IdentifierToken "_xs"], "")
      lexerRun "__x_s" `shouldBe` ([IdentifierToken "__x_s"], "")
      lexerRun "x0" `shouldBe` ([IdentifierToken "x0"], "")
      lexerRun "_0" `shouldBe` ([IdentifierToken "_0"], "")
      lexerRun "_Jdwioo8__282" `shouldBe` ([IdentifierToken "_Jdwioo8__282"], "")
    it "doesn't lex unknown chars" $ do
      lexerRun "-" `shouldBe` ([], "-")
      lexerRun "@" `shouldBe` ([], "@")
      lexerRun "$" `shouldBe` ([], "$")
      lexerRun "*" `shouldBe` ([], "*")
      lexerRun "-@$*" `shouldBe` ([], "-@$*")
    it "stops lexing on mismatch" $ do
      lexerRun "ADD R1 R1 R2 - label:" `shouldBe` ([OpcodeToken ADD,
                                                    RegisterToken R1,
                                                    RegisterToken R1,
                                                    RegisterToken R2],
                                                    "- label:")

