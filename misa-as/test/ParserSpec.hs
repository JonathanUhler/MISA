module ParserSpec (spec) where


import Parser
import Grammar

import Data.Void
import Text.Megaparsec
import Test.Hspec


parseInst :: String -> Either (ParseErrorBundle String Void) Program
parseInst s = runParser parseProgram "" s


parsesAs :: String -> Inst -> Expectation
parsesAs s expected = do
  result <- case parseInst s of
    Right (InstStatement inst : _) -> pure inst
    Right _ -> fail "Did not parse as instruction"
    Left err -> fail (errorBundlePretty err)
  result `shouldBe` expected


parsesAsLabel :: String -> String -> Expectation
parsesAsLabel s expected = do
  result <- case parseInst s of
    Right (LabelStatement name : _) -> pure name
    Right _ -> fail "Did not parse as label"
    Left err -> fail (errorBundlePretty err)
  result `shouldBe` expected


parsesAsDir :: String -> Dir -> Expectation
parsesAsDir s expected = do
  result <- case parseInst s of
    Right (DirStatement dir : _) -> pure dir
    Right _ -> fail "Did not parse as directive"
    Left err -> fail (errorBundlePretty err)
  result `shouldBe` expected


failsToParse :: String -> Expectation
failsToParse s = do
  result <- case parseInst s of
    Right _ -> fail "Expected parse to fail but succeeded"
    Left _ -> pure ()
  result `shouldBe` ()


spec :: Spec
spec = do
  describe "ADD" $ do
    it "parses add with all same registers" $
      "add r0 r0 r0" `parsesAs` AddInst R0 R0 R0
    it "parses add with different registers" $
      "add ra rb rc" `parsesAs` AddInst RA RB RC
    it "parses add with all registers (first set)" $
      "add r0 ra rb" `parsesAs` AddInst R0 RA RB
    it "parses add with all registers (second set)" $
      "add rc rd re" `parsesAs` AddInst RC RD RE
    it "parses add with all registers (third set)" $
      "add rf rg rh" `parsesAs` AddInst RF RG RH
    it "parses add with all registers (fourth set)" $
      "add ru rv rw" `parsesAs` AddInst RU RV RW
    it "parses add with all registers (fifth set)" $
      "add rx ry rz" `parsesAs` AddInst RX RY RZ
    it "parses add with RT register" $
      "add rt r0 r0" `parsesAs` AddInst RT R0 R0
    it "parses add case-insensitively" $
      "ADD RA RB RC" `parsesAs` AddInst RA RB RC
    it "parses add with whitespace variations" $
      "add  ra   rb    rc" `parsesAs` AddInst RA RB RC

  describe "ADC" $ do
    it "parses adc with all same registers" $
      "adc r0 r0 r0" `parsesAs` AdcInst R0 R0 R0
    it "parses adc with different registers" $
      "adc ra rb rc" `parsesAs` AdcInst RA RB RC
    it "parses adc case-insensitively" $
      "ADC RC RD RE" `parsesAs` AdcInst RC RD RE

  describe "SUB" $ do
    it "parses sub with all same registers" $
      "sub r0 r0 r0" `parsesAs` SubInst R0 R0 R0
    it "parses sub with different registers" $
      "sub ra rb rc" `parsesAs` SubInst RA RB RC
    it "parses sub with all registers combinations" $
      "sub rg rh ru" `parsesAs` SubInst RG RH RU

  describe "SBB" $ do
    it "parses sbb with all same registers" $
      "sbb r0 r0 r0" `parsesAs` SbbInst R0 R0 R0
    it "parses sbb with different registers" $
      "sbb ra rb rc" `parsesAs` SbbInst RA RB RC
    it "parses sbb case-insensitively" $
      "SBB RV RW RX" `parsesAs` SbbInst RV RW RX

  describe "AND" $ do
    it "parses and with all same registers" $
      "and r0 r0 r0" `parsesAs` AndInst R0 R0 R0
    it "parses and with different registers" $
      "and ra rb rc" `parsesAs` AndInst RA RB RC
    it "parses and with all registers" $
      "and ry rz rt" `parsesAs` AndInst RY RZ RT

  describe "OR" $ do
    it "parses or with all same registers" $
      "or r0 r0 r0" `parsesAs` OrInst R0 R0 R0
    it "parses or with different registers" $
      "or ra rb rc" `parsesAs` OrInst RA RB RC
    it "parses or with all registers" $
      "or rf rg rh" `parsesAs` OrInst RF RG RH

  describe "XOR" $ do
    it "parses xor with all same registers" $
      "xor r0 r0 r0" `parsesAs` XorInst R0 R0 R0
    it "parses xor with different registers" $
      "xor ra rb rc" `parsesAs` XorInst RA RB RC
    it "parses xor with all registers" $
      "xor ru rv rw" `parsesAs` XorInst RU RV RW

  describe "RRC" $ do
    it "parses rrc with same register" $
      "rrc r0 r0" `parsesAs` RrcInst R0 R0
    it "parses rrc with different registers" $
      "rrc ra rb" `parsesAs` RrcInst RA RB
    it "parses rrc with all registers (first)" $
      "rrc rc rd" `parsesAs` RrcInst RC RD
    it "parses rrc with all registers (second)" $
      "rrc re rf" `parsesAs` RrcInst RE RF
    it "parses rrc with all registers (third)" $
      "rrc rg rh" `parsesAs` RrcInst RG RH
    it "parses rrc with all registers (fourth)" $
      "rrc ru rv" `parsesAs` RrcInst RU RV
    it "parses rrc with all registers (fifth)" $
      "rrc rw rx" `parsesAs` RrcInst RW RX
    it "parses rrc with all registers (sixth)" $
      "rrc ry rz" `parsesAs` RrcInst RY RZ
    it "parses rrc with RT register" $
      "rrc rt r0" `parsesAs` RrcInst RT R0
    it "parses rrc case-insensitively" $
      "RRC RA RB" `parsesAs` RrcInst RA RB

  describe "LW" $ do
    it "parses lw with single registers" $
      "lw ra rb rc" `parsesAs` LwInst RA RB RC
    it "parses lw with all same registers" $
      "lw r0 r0 r0" `parsesAs` LwInst R0 R0 R0
    it "parses lw with wide register RAB" $
      "lw rc rab" `parsesAs` LwInst RC RA RB
    it "parses lw with wide register RCD" $
      "lw re rcd" `parsesAs` LwInst RE RC RD
    it "parses lw with wide register REF" $
      "lw rg ref" `parsesAs` LwInst RG RE RF
    it "parses lw with wide register RGH" $
      "lw rh rgh" `parsesAs` LwInst RH RG RH
    it "parses lw with wide register RUV" $
      "lw ra ruv" `parsesAs` LwInst RA RU RV
    it "parses lw with wide register RWX" $
      "lw rb rwx" `parsesAs` LwInst RB RW RX
    it "parses lw with wide register RYZ" $
      "lw rc ryz" `parsesAs` LwInst RC RY RZ
    it "parses lw case-insensitively" $
      "LW RA RB RC" `parsesAs` LwInst RA RB RC

  describe "SW" $ do
    it "parses sw with single registers" $
      "sw ra rb rc" `parsesAs` SwInst RA RB RC
    it "parses sw with all same registers" $
      "sw r0 r0 r0" `parsesAs` SwInst R0 R0 R0
    it "parses sw with wide register RAB" $
      "sw rd rab" `parsesAs` SwInst RD RA RB
    it "parses sw with wide register RCD" $
      "sw re rcd" `parsesAs` SwInst RE RC RD
    it "parses sw with wide register REF" $
      "sw rf ref" `parsesAs` SwInst RF RE RF
    it "parses sw with wide register RGH" $
      "sw rg rgh" `parsesAs` SwInst RG RG RH
    it "parses sw with wide register RUV" $
      "sw rh ruv" `parsesAs` SwInst RH RU RV
    it "parses sw with wide register RWX" $
      "sw ru rwx" `parsesAs` SwInst RU RW RX
    it "parses sw with wide register RYZ" $
      "sw rv ryz" `parsesAs` SwInst RV RY RZ
    it "parses sw case-insensitively" $
      "SW RA RB RC" `parsesAs` SwInst RA RB RC

  describe "RSR" $ do
    it "parses rsr with SADDR" $
      "rsr saddr ra rb" `parsesAs` RsrInst SADDR RA RB
    it "parses rsr with RADDR" $
      "rsr raddr rc rd" `parsesAs` RsrInst RADDR RC RD
    it "parses rsr with FLAGS" $
      "rsr flags re rf" `parsesAs` RsrInst FLAGS RE RF
    it "parses rsr with CAUSE" $
      "rsr cause rg rh" `parsesAs` RsrInst CAUSE RG RH
    it "parses rsr with all registers" $
      "rsr saddr ru rv" `parsesAs` RsrInst SADDR RU RV
    it "parses rsr case-insensitively" $
      "RSR SADDR RA RB" `parsesAs` RsrInst SADDR RA RB

  describe "WSR" $ do
    it "parses wsr with SADDR" $
      "wsr saddr ra rb" `parsesAs` WsrInst SADDR RA RB
    it "parses wsr with RADDR" $
      "wsr raddr rc rd" `parsesAs` WsrInst RADDR RC RD
    it "parses wsr with FLAGS" $
      "wsr flags re rf" `parsesAs` WsrInst FLAGS RE RF
    it "parses wsr with CAUSE" $
      "wsr cause rg rh" `parsesAs` WsrInst CAUSE RG RH
    it "parses wsr with all registers" $
      "wsr saddr rw rx" `parsesAs` WsrInst SADDR RW RX
    it "parses wsr case-insensitively" $
      "WSR RADDR RC RD" `parsesAs` WsrInst RADDR RC RD

  describe "SET" $ do
    it "parses set with decimal immediate" $
      "set ra 42" `parsesAs` SetInst RA (IntImm Low 42)
    it "parses set with zero" $
      "set rb 0" `parsesAs` SetInst RB (IntImm Low 0)
    it "parses set with maximum word value" $
      "set rc 255" `parsesAs` SetInst RC (IntImm Low 255)
    it "parses set with hex immediate" $
      "set rd 0xFF" `parsesAs` SetInst RD (IntImm Low 255)
    it "parses set with binary immediate" $
      "set re 0b11111111" `parsesAs` SetInst RE (IntImm Low 255)
    it "parses set with octal immediate" $
      "set rf 0o377" `parsesAs` SetInst RF (IntImm Low 255)
    it "parses set with label" $
      "set rg myLabel" `parsesAs` SetInst RG (LabelImm Low "myLabel")
    it "parses set with label case-insensitively" $
      "set rh another_label" `parsesAs` SetInst RH (LabelImm Low "another_label")
    it "parses set with underscores in label" $
      "set ru _label_123" `parsesAs` SetInst RU (LabelImm Low "_label_123")
    it "parses set case-insensitively" $
      "SET RV 99" `parsesAs` SetInst RV (IntImm Low 99)

  describe "JAL" $ do
    it "parses jal with ALWAYS condition" $
      "jal always ra rb" `parsesAs` JalInst ALWAYS RA RB
    it "parses jal with EQUAL condition" $
      "jal equal rc rd" `parsesAs` JalInst EQUAL RC RD
    it "parses jal with NOT_EQUAL condition" $
      "jal not_equal re rf" `parsesAs` JalInst NOT_EQUAL RE RF
    it "parses jal with GREATER condition" $
      "jal greater rg rh" `parsesAs` JalInst GREATER RG RH
    it "parses jal with LESS condition" $
      "jal less ru rv" `parsesAs` JalInst LESS RU RV
    it "parses jal with GREATER_EQUAL condition" $
      "jal greater_equal rw rx" `parsesAs` JalInst GREATER_EQUAL RW RX
    it "parses jal with LESS_EQUAL condition" $
      "jal less_equal ry rz" `parsesAs` JalInst LESS_EQUAL RY RZ
    it "parses jal with all registers" $
      "jal always rt r0" `parsesAs` JalInst ALWAYS RT R0
    it "parses jal case-insensitively" $
      "JAL EQUAL RA RB" `parsesAs` JalInst EQUAL RA RB

  describe "JMP" $ do
    it "parses jmp with ALWAYS condition" $
      "jmp always ra rb" `parsesAs` JmpInst ALWAYS RA RB
    it "parses jmp with EQUAL condition" $
      "jmp equal rc rd" `parsesAs` JmpInst EQUAL RC RD
    it "parses jmp with NOT_EQUAL condition" $
      "jmp not_equal re rf" `parsesAs` JmpInst NOT_EQUAL RE RF
    it "parses jmp with GREATER condition" $
      "jmp greater rg rh" `parsesAs` JmpInst GREATER RG RH
    it "parses jmp with LESS condition" $
      "jmp less ru rv" `parsesAs` JmpInst LESS RU RV
    it "parses jmp with GREATER_EQUAL condition" $
      "jmp greater_equal rw rx" `parsesAs` JmpInst GREATER_EQUAL RW RX
    it "parses jmp with LESS_EQUAL condition" $
      "jmp less_equal ry rz" `parsesAs` JmpInst LESS_EQUAL RY RZ
    it "parses jmp with all registers" $
      "jmp always rt r0" `parsesAs` JmpInst ALWAYS RT R0
    it "parses jmp case-insensitively" $
      "JMP NOT_EQUAL RE RF" `parsesAs` JmpInst NOT_EQUAL RE RF

  describe "HALT" $ do
    it "parses halt with r0" $
      "halt r0" `parsesAs` HaltInst R0
    it "parses halt with any register" $
      "halt ra" `parsesAs` HaltInst RA
    it "parses halt with register from each set" $ do
      "halt rb" `parsesAs` HaltInst RB
      "halt rc" `parsesAs` HaltInst RC
      "halt rg" `parsesAs` HaltInst RG
      "halt ru" `parsesAs` HaltInst RU
      "halt rz" `parsesAs` HaltInst RZ
    it "parses halt with RT" $
      "halt rt" `parsesAs` HaltInst RT
    it "parses halt case-insensitively" $
      "HALT RA" `parsesAs` HaltInst RA

  describe "NOP" $ do
    it "parses nop" $
      "nop" `parsesAs` NopInst
    it "parses nop case-insensitively" $
      "NOP" `parsesAs` NopInst
    it "parses nop with whitespace" $
      "  nop  " `parsesAs` NopInst

  describe "MOV" $ do
    it "parses mov with same registers" $
      "mov r0 r0" `parsesAs` MovInst R0 R0
    it "parses mov with different registers" $
      "mov ra rb" `parsesAs` MovInst RA RB
    it "parses mov with all register combinations (first)" $
      "mov rc rd" `parsesAs` MovInst RC RD
    it "parses mov with all register combinations (second)" $
      "mov re rf" `parsesAs` MovInst RE RF
    it "parses mov with all register combinations (third)" $
      "mov rg rh" `parsesAs` MovInst RG RH
    it "parses mov with all register combinations (fourth)" $
      "mov ru rv" `parsesAs` MovInst RU RV
    it "parses mov with all register combinations (fifth)" $
      "mov rw rx" `parsesAs` MovInst RW RX
    it "parses mov with all register combinations (sixth)" $
      "mov ry rz" `parsesAs` MovInst RY RZ
    it "parses mov with RT" $
      "mov rt r0" `parsesAs` MovInst RT R0
    it "parses mov case-insensitively" $
      "MOV RA RB" `parsesAs` MovInst RA RB

  describe "CMP" $ do
    it "parses cmp with same registers" $
      "cmp r0 r0" `parsesAs` CmpInst R0 R0
    it "parses cmp with different registers" $
      "cmp ra rb" `parsesAs` CmpInst RA RB
    it "parses cmp with all register combinations (first)" $
      "cmp rc rd" `parsesAs` CmpInst RC RD
    it "parses cmp with all register combinations (second)" $
      "cmp re rf" `parsesAs` CmpInst RE RF
    it "parses cmp with all register combinations (third)" $
      "cmp rg rh" `parsesAs` CmpInst RG RH
    it "parses cmp with all register combinations (fourth)" $
      "cmp ru rv" `parsesAs` CmpInst RU RV
    it "parses cmp with all register combinations (fifth)" $
      "cmp rw rx" `parsesAs` CmpInst RW RX
    it "parses cmp with all register combinations (sixth)" $
      "cmp ry rz" `parsesAs` CmpInst RY RZ
    it "parses cmp with RT" $
      "cmp rt r0" `parsesAs` CmpInst RT R0
    it "parses cmp case-insensitively" $
      "CMP RA RB" `parsesAs` CmpInst RA RB

  describe "SET2" $ do
    it "parses set2 with single registers and decimal immediate" $
      "set2 ra rb 1000" `parsesAs` Set2Inst RA RB (IntImm Full 1000)
    it "parses set2 with wide register RAB" $
      "set2 rab 42" `parsesAs` Set2Inst RA RB (IntImm Full 42)
    it "parses set2 with wide register RCD" $
      "set2 rcd 100" `parsesAs` Set2Inst RC RD (IntImm Full 100)
    it "parses set2 with wide register REF" $
      "set2 ref 200" `parsesAs` Set2Inst RE RF (IntImm Full 200)
    it "parses set2 with wide register RGH" $
      "set2 rgh 300" `parsesAs` Set2Inst RG RH (IntImm Full 300)
    it "parses set2 with wide register RUV" $
      "set2 ruv 400" `parsesAs` Set2Inst RU RV (IntImm Full 400)
    it "parses set2 with wide register RWX" $
      "set2 rwx 500" `parsesAs` Set2Inst RW RX (IntImm Full 500)
    it "parses set2 with wide register RYZ" $
      "set2 ryz 600" `parsesAs` Set2Inst RY RZ (IntImm Full 600)
    it "parses set2 with hex immediate" $
      "set2 ra rb 0xFFFF" `parsesAs` Set2Inst RA RB (IntImm Full 0xFFFF)
    it "parses set2 with binary immediate" $
      "set2 rc rd 0b1111000011110000" `parsesAs` Set2Inst RC RD (IntImm Full 0xF0F0)
    it "parses set2 with octal immediate" $
      "set2 re rf 0o177777" `parsesAs` Set2Inst RE RF (IntImm Full 65535)
    it "parses set2 with label" $
      "set2 rg rh myLabel" `parsesAs` Set2Inst RG RH (LabelImm Full "myLabel")
    it "parses set2 case-insensitively" $
      "SET2 RA RB 5000" `parsesAs` Set2Inst RA RB (IntImm Full 5000)

  describe "CALL" $ do
    it "parses call with single registers" $
      "call ra rb" `parsesAs` CallInst RA RB
    it "parses call with wide register RAB" $
      "call rab" `parsesAs` CallInst RA RB
    it "parses call with wide register RCD" $
      "call rcd" `parsesAs` CallInst RC RD
    it "parses call with wide register REF" $
      "call ref" `parsesAs` CallInst RE RF
    it "parses call with wide register RGH" $
      "call rgh" `parsesAs` CallInst RG RH
    it "parses call with wide register RUV" $
      "call ruv" `parsesAs` CallInst RU RV
    it "parses call with wide register RWX" $
      "call rwx" `parsesAs` CallInst RW RX
    it "parses call with wide register RYZ" $
      "call ryz" `parsesAs` CallInst RY RZ
    it "parses call case-insensitively" $
      "CALL RA RB" `parsesAs` CallInst RA RB

  describe "RET" $ do
    it "parses ret" $
      "ret" `parsesAs` RetInst
    it "parses ret case-insensitively" $
      "RET" `parsesAs` RetInst
    it "parses ret with whitespace" $
      "  ret  " `parsesAs` RetInst

  describe "CLR" $ do
    it "parses clr" $
      "clr" `parsesAs` ClrInst
    it "parses clr case-insensitively" $
      "CLR" `parsesAs` ClrInst
    it "parses clr with whitespace" $
      "  clr  " `parsesAs` ClrInst

  describe "Labels" $ do
    it "parses simple label" $
      "start:" `parsesAsLabel` "start"
    it "parses label with underscores" $
      "_label:" `parsesAsLabel` "_label"
    it "parses label with numbers" $
      "loop1:" `parsesAsLabel` "loop1"
    it "parses label with mixed case" $
      "MyLabel:" `parsesAsLabel` "MyLabel"
    it "parses label with underscores and numbers" $
      "label_123:" `parsesAsLabel` "label_123"
    it "parses label with all underscores" $
      "___:" `parsesAsLabel` "___"
    it "parses label with leading underscore" $
      "_:" `parsesAsLabel` "_"

  describe ".word" $ do
    it "parses .word with zero" $
      ".word 0" `parsesAsDir` WordDir 0
    it "parses .word with small value" $
      ".word 42" `parsesAsDir` WordDir 42
    it "parses .word with maximum byte value" $
      ".word 255" `parsesAsDir` WordDir 255
    it "parses .word with hex value" $
      ".word 0xFF" `parsesAsDir` WordDir 255
    it "parses .word with binary value" $
      ".word 0b11111111" `parsesAsDir` WordDir 255
    it "parses .word with octal value" $
      ".word 0o377" `parsesAsDir` WordDir 255
    it "parses .word case-insensitively" $
      ".WORD 100" `parsesAsDir` WordDir 100

  describe ".array" $ do
    it "parses .array with single byte" $
      ".array 42" `parsesAsDir` ArrayDir [42]
    it "parses .array with multiple bytes" $
      ".array 0 1 2" `parsesAsDir` ArrayDir [0, 1, 2]
    it "parses .array with many bytes" $
      ".array 255 254 253 1 0" `parsesAsDir` ArrayDir [255, 254, 253, 1, 0]
    it "parses .array with hex values" $
      ".array 0xFF 0x00 0xAA" `parsesAsDir` ArrayDir [255, 0, 170]
    it "parses .array with binary values" $
      ".array 0b11111111 0b00000000" `parsesAsDir` ArrayDir [255, 0]
    it "parses .array with octal values" $
      ".array 0o377 0o000" `parsesAsDir` ArrayDir [255, 0]
    it "parses .array with mixed formats" $
      ".array 255 0xFF 0b11111111 0o377" `parsesAsDir` ArrayDir [255, 255, 255, 255]
    it "parses .array case-insensitively" $
      ".ARRAY 10 20 30" `parsesAsDir` ArrayDir [10, 20, 30]

  describe ".section" $ do
    it "parses .section with simple name" $
      ".section text" `parsesAsDir` SectionDir "text"
    it "parses .section with underscores" $
      ".section my_section" `parsesAsDir` SectionDir "my_section"
    it "parses .section with numbers" $
      ".section section1" `parsesAsDir` SectionDir "section1"
    it "parses .section with leading underscore" $
      ".section _section" `parsesAsDir` SectionDir "_section"
    it "parses .section with mixed case" $
      ".section MySection" `parsesAsDir` SectionDir "MySection"
    it "parses .section case-insensitively" $
      ".SECTION data" `parsesAsDir` SectionDir "data"

  describe "Comments" $ do
    it "parses instruction with line comment" $
      "add ra rb rc // comment" `parsesAs` AddInst RA RB RC
    it "parses instruction with block comment" $
      "add ra rb rc /* comment */" `parsesAs` AddInst RA RB RC
    it "parses instruction with block comment before" $
      "/* comment */ add ra rb rc" `parsesAs` AddInst RA RB RC
    it "parses label with line comment" $
      "start: // comment" `parsesAsLabel` "start"
    it "parses label with block comment" $
      "start: /* comment */" `parsesAsLabel` "start"
    it "parses directive with line comment" $
      ".word 42 // comment" `parsesAsDir` WordDir 42

  describe "Whitespace" $ do
    it "parses with leading whitespace" $
      "  add ra rb rc" `parsesAs` AddInst RA RB RC
    it "parses with trailing whitespace" $
      "add ra rb rc  " `parsesAs` AddInst RA RB RC
    it "parses with tabs" $
      "add\tra\trb\trc" `parsesAs` AddInst RA RB RC
    it "parses with mixed whitespace" $
      "  add  \t ra  rb  rc  " `parsesAs` AddInst RA RB RC

  describe "Multiple statements" $ do
    it "parses two instructions" $ do
      case parseInst "add ra rb rc add rd re rf" of
        Right (InstStatement inst1 : InstStatement inst2 : _) -> do
          inst1 `shouldBe` AddInst RA RB RC
          inst2 `shouldBe` AddInst RD RE RF
        _ -> fail "Expected two instructions"
    it "parses label followed by instruction" $ do
      case parseInst "start: add ra rb rc" of
        Right (LabelStatement name : InstStatement inst : _) -> do
          name `shouldBe` "start"
          inst `shouldBe` AddInst RA RB RC
        _ -> fail "Expected label then instruction"
    it "parses instruction followed by directive" $ do
      case parseInst "add ra rb rc .word 42" of
        Right (InstStatement inst : DirStatement dir : _) -> do
          inst `shouldBe` AddInst RA RB RC
          dir `shouldBe` WordDir 42
        _ -> fail "Expected instruction then directive"
    it "parses label, instruction, and directive" $ do
      case parseInst "start: mov ra rb .section text" of
        Right (LabelStatement name : InstStatement inst : DirStatement dir : _) -> do
          name `shouldBe` "start"
          inst `shouldBe` MovInst RA RB
          dir `shouldBe` SectionDir "text"
        _ -> fail "Expected label, instruction, and directive"
    it "parses complex sequence with comments" $ do
      case parseInst "start: /* begin */ add ra rb rc // comment\n.word 100" of
        Right (LabelStatement name : InstStatement inst : DirStatement dir : _) -> do
          name `shouldBe` "start"
          inst `shouldBe` AddInst RA RB RC
          dir `shouldBe` WordDir 100
        _ -> fail "Expected label, instruction, and directive"

  describe "Errors" $ do
    it "rejects invalid register name" $
      failsToParse "add r0 r1 r2"
    it "rejects instruction with missing operand" $
      failsToParse "add ra rb"
    it "rejects instruction with too many operands" $
      failsToParse "add ra rb rc rd"
    it "rejects reserved word as label" $
      failsToParse "add:"
    it "rejects reserved word as identifier" $
      failsToParse ".section add"
    it "rejects invalid directive" $
      failsToParse ".invalid 42"
    it "rejects integer out of Word8 range" $
      failsToParse ".word 256"
    it "rejects negative integer" $
      failsToParse ".word -1"
    it "rejects empty program" $
      failsToParse ""
    it "rejects unknown instruction" $
      failsToParse "invalid ra rb rc"
