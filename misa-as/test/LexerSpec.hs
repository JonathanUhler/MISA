module LexerSpec (spec) where


import Grammar
import Lexer

import Test.Hspec


spec :: Spec
spec = do
  describe "lexerRun" $ do
    it "lexes nothing" $
      lexerRun "" `shouldBe` []
    it "lexes whitespace" $
      lexerRun "\n\r\t " `shouldBe` []
    it "lexes comments" $
      lexerRun "; comment" `shouldBe` []
    it "lexes nested comments" $
      lexerRun "; comment ; another semicolon" `shouldBe` []
    it "lexes colon" $
      lexerRun ":" `shouldBe` [TokenColon]
    it "lexes period" $
      lexerRun "." `shouldBe` [TokenPeriod]
    it "lexes opcodes" $ do
      lexerRun "ADD" `shouldBe` [TokenOpcode ADD]
      lexerRun "ADC" `shouldBe` [TokenOpcode ADC]
      lexerRun "SUB" `shouldBe` [TokenOpcode SUB]
      lexerRun "AND" `shouldBe` [TokenOpcode AND]
      lexerRun "OR" `shouldBe` [TokenOpcode OR]
      lexerRun "XOR" `shouldBe` [TokenOpcode XOR]
      lexerRun "LW" `shouldBe` [TokenOpcode LW]
      lexerRun "SW" `shouldBe` [TokenOpcode SW]
      lexerRun "LA" `shouldBe` [TokenOpcode LA]
      lexerRun "SA" `shouldBe` [TokenOpcode SA]
      lexerRun "LI" `shouldBe` [TokenOpcode LI]
      lexerRun "JLZ" `shouldBe` [TokenOpcode JLZ]
      lexerRun "HALT" `shouldBe` [TokenOpcode HALT]
    it "lexes registers" $ do
      lexerRun "R0" `shouldBe` [TokenRegister R0]
      lexerRun "R1" `shouldBe` [TokenRegister R1]
      lexerRun "R2" `shouldBe` [TokenRegister R2]
      lexerRun "R3" `shouldBe` [TokenRegister R3]
      lexerRun "R4" `shouldBe` [TokenRegister R4]
      lexerRun "R5" `shouldBe` [TokenRegister R5]
      lexerRun "R6" `shouldBe` [TokenRegister R6]
      lexerRun "R7" `shouldBe` [TokenRegister R7]
      lexerRun "R8" `shouldBe` [TokenRegister R8]
      lexerRun "R9" `shouldBe` [TokenRegister R9]
      lexerRun "R10" `shouldBe` [TokenRegister R10]
      lexerRun "R11" `shouldBe` [TokenRegister R11]
      lexerRun "R12" `shouldBe` [TokenRegister R12]
      lexerRun "R13" `shouldBe` [TokenRegister R13]
      lexerRun "R14" `shouldBe` [TokenRegister R14]
      lexerRun "R15" `shouldBe` [TokenRegister R15]
    it "lexes single-digit numbers" $ do
      lexerRun "0x1" `shouldBe` [TokenNumber 1]
      lexerRun "0b1" `shouldBe` [TokenNumber 1]
      lexerRun "0o1" `shouldBe` [TokenNumber 1]
      lexerRun "1" `shouldBe` [TokenNumber 1]
    it "lexes multi-digit numbers" $ do
      lexerRun "0x10" `shouldBe` [TokenNumber 16]
      lexerRun "0b10" `shouldBe` [TokenNumber 2]
      lexerRun "0o10" `shouldBe` [TokenNumber 8]
      lexerRun "10" `shouldBe` [TokenNumber 10]
      lexerRun "-10" `shouldBe` [TokenUnknown "-10"]
    it "lexes identifiers" $ do
      lexerRun "_" `shouldBe` [TokenIdentifier "_"]
      lexerRun "x" `shouldBe` [TokenIdentifier "x"]
      lexerRun "X" `shouldBe` [TokenIdentifier "X"]
      lexerRun "xs" `shouldBe` [TokenIdentifier "xs"]
      lexerRun "Xs" `shouldBe` [TokenIdentifier "Xs"]
      lexerRun "_xs" `shouldBe` [TokenIdentifier "_xs"]
      lexerRun "__x_s" `shouldBe` [TokenIdentifier "__x_s"]
      lexerRun "x0" `shouldBe` [TokenIdentifier "x0"]
      lexerRun "_0" `shouldBe` [TokenIdentifier "_0"]
    it "doesn't lex unknown chars" $ do
      lexerRun "-" `shouldBe` [TokenUnknown "-"]
      lexerRun "@" `shouldBe` [TokenUnknown "@"]
      lexerRun "$" `shouldBe` [TokenUnknown "$"]
      lexerRun "*" `shouldBe` [TokenUnknown "*"]
      lexerRun "-@$*" `shouldBe` [TokenUnknown "-@$*"]
    it "stops lexing on mismatch" $ do
      lexerRun "ADD R1 R1 R2 - label:" `shouldBe` [TokenOpcode ADD,
                                                    TokenRegister R1,
                                                    TokenRegister R1,
                                                    TokenRegister R2,
                                                    TokenUnknown "- label:"]

