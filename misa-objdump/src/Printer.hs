module Printer (printProgram) where


import Grammar


printProgram :: Program -> String
printProgram stats = unlines (map printStat stats)


printStat :: Stat -> String
printStat (InstStat inst)   = instStr
  where instStr = case inst of
          HaltInst rs          -> unwords ["HALT", show rs]
          AddInst  rd  rs1 rs2 -> unwords ["ADD",  show rd,  show rs1, show rs2]
          AdcInst  rd  rs1 rs2 -> unwords ["ADC",  show rd,  show rs1, show rs2]
          SubInst  rd  rs1 rs2 -> unwords ["SUB",  show rd,  show rs1, show rs2]
          SbbInst  rd  rs1 rs2 -> unwords ["SBB",  show rd,  show rs1, show rs2]
          AndInst  rd  rs1 rs2 -> unwords ["AND",  show rd,  show rs1, show rs2]
          OrInst   rd  rs1 rs2 -> unwords ["OR",   show rd,  show rs1, show rs2]
          XorInst  rd  rs1 rs2 -> unwords ["XOR",  show rd,  show rs1, show rs2]
          RrcInst  rd  rs      -> unwords ["RRC",  show rd,  show rs]
          SetInst  rd  imm     -> unwords ["SET",  show rd,  showImm imm]
          LdInst   rd  rs1 rs2 -> unwords ["LD",   show rd,  show rs1, show rs2]
          StInst   rd  rs1 rs2 -> unwords ["ST",   show rd,  show rs1, show rs2]
          RsrInst  csr rs1 rs2 -> unwords ["RSR",  show csr, show rs1, show rs2]
          WsrInst  csr rs1 rs2 -> unwords ["WSR",  show csr, show rs1, show rs2]
          JalInst  cmp rs1 rs2 -> unwords ["JAL",  show cmp, show rs1, show rs2]
          JmpInst  cmp rs1 rs2 -> unwords ["JMP",  show cmp, show rs1, show rs2]
          -- Pseudo instructions
          Add2Inst rd1 rd2 rs1 rs2 rs3 rs4
            -> unwords ["ADD2", show rd1, show rd2, show rs1, show rs2, show rs3, show rs4]
          And2Inst rd1 rd2 rs1 rs2 rs3 rs4
            -> unwords ["AND2", show rd1, show rd2, show rs1, show rs2, show rs3, show rs4]
          CallInst imm         -> unwords ["CALL",  showImm imm]
          ClrInst              -> "CLR"
          CmpInst  rs1 rs2     -> unwords ["CMP",   show rs1, show rs2]
          GotoInst imm         -> unwords ["GOTO",  showImm imm]
          JaliInst cmp imm     -> unwords ["JALI",  show cmp, showImm imm]
          JmpiInst cmp imm     -> unwords ["JMPI",  show cmp, showImm imm]
          Ld2Inst  rd1 rd2 rs1 rs2
            -> unwords ["LD2", show rd1, show rd2,  show rs1, show rs2]
          MovInst   rd  rs     -> unwords ["MOV",   show rd,  show rs]
          Mov2Inst  rd1 rd2 rs1 rs2
            -> unwords ["MOV2", show rd1, show rd2, show rs1, show rs2]
          NopInst              -> "NOP"
          Or2Inst   rd1 rd2 rs1 rs2 rs3 rs4
            -> unwords ["or2", show rd1, show rd2, show rs1, show rs2, show rs3, show rs4]
          PopInst   rd         -> unwords ["POP",   show rd]
          Pop2Inst  rd1 rd2    -> unwords ["POP2",  show rd1, show rd2]
          PushInst  rs         -> unwords ["PUSH",  show rs]
          Push2Inst rs1 rs2    -> unwords ["PUSH2", show rs1, show rs2]
          RetInst              -> "RET"
          Rrc2Inst  rd1 rd2 rs1 rs2
            -> unwords ["RRC2", show rd1, show rd2, show rs1, show rs2]
          Set2Inst  rd1 rd2 imm
            -> unwords ["SET2", show rd1, show rd2, showImm imm]
          St2Inst   rd1 rd2 rs1 rs2
            -> unwords ["ST2", show rd1, show rd2, show rs1, show rs2]
          Sub2Inst  rd1 rd2 rs1 rs2 rs3 rs4
            -> unwords ["SUB2", show rd1, show rd2, show rs1, show rs2, show rs3, show rs4]
          Xor2Inst  rd1 rd2 rs1 rs2 rs3 rs4
            -> unwords ["XOR2", show rd1, show rd2, show rs1, show rs2, show rs3, show rs4]
          -- Syscall extension
          SyscallInst rs       -> unwords ["SYSCALL", show rs]
          RetsInst             -> "RETS"
printStat (LabelStat label) = labelStr
  where labelStr = show label ++ ":"
printStat (DirStat dir)     = dirStr
  where dirStr = "." ++ name ++ " " ++ operands
        (name, operands) = case dir of
          WordDir word     -> ("word",    show word)
          ArrayDir array   -> ("array",   show array)
          AddrDir label    -> ("addr",    label)
          AsciiDir string  -> ("ascii",   string)
          AsciizDir string -> ("asciiz",  string)
          SpaceDir count   -> ("space",   show count)
          SectionDir label -> ("section", label)


showImm :: Imm -> String
showImm (IntImm _ int)     = show int
showImm (LabelImm _ label) = label
