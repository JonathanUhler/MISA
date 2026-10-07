import MisaSpec.Basic
import MisaSpec.GpReg
import MisaSpec.CsrReg
import MisaSpec.CmpFlag


namespace MisaSpec


inductive Inst where
  | halt   (rs : GpReg)
  | add    (rd : GpReg)   (rs1 : GpReg)    (rs2 : GpReg)
  | adc    (rd : GpReg)   (rs1 : GpReg)    (rs2 : GpReg)
  | sub    (rd : GpReg)   (rs1 : GpReg)    (rs2 : GpReg)
  | sbb    (rd : GpReg)   (rs1 : GpReg)    (rs2 : GpReg)
  | and    (rd : GpReg)   (rs1 : GpReg)    (rs2 : GpReg)
  | or     (rd : GpReg)   (rs1 : GpReg)    (rs2 : GpReg)
  | xor    (rd : GpReg)   (rs1 : GpReg)    (rs2 : GpReg)
  | rrc    (rd : GpReg)   (rs : GpReg)
  | set    (rd : GpReg)   (imm : DataWord)
  | ld     (rd : GpReg)   (rsb : GpReg)    (imm : HalfWord)
  | st     (rd : GpReg)   (rsb : GpReg)    (imm : HalfWord)
  | rsr    (rs1 : GpReg)  (rs2 : GpReg)    (csr : CsrReg)
  | wsr    (rs1 : GpReg)  (rs2 : GpReg)    (csr : CsrReg)
  | jal    (rs1 : GpReg)  (rs2 : GpReg)    (cmp : CmpFlag)
  | jmp    (rs1 : GpReg)  (rs2 : GpReg)    (cmp : CmpFlag)
  | jmpCsr (csr : CsrReg) (cmp : CmpFlag)
  deriving Repr


def Inst.enc : Inst -> InstWord
  | .halt   rs          =>                 0b00000000#8 ++  rs.enc ++ 0b0000#4
  | .add    rd  rs1 rs2 =>          rs2.enc ++  rs1.enc ++  rd.enc ++ 0b0001#4
  | .adc    rd  rs1 rs2 =>          rs2.enc ++  rs1.enc ++  rd.enc ++ 0b0010#4
  | .sub    rd  rs1 rs2 =>          rs2.enc ++  rs1.enc ++  rd.enc ++ 0b0011#4
  | .sbb    rd  rs1 rs2 =>          rs2.enc ++  rs1.enc ++  rd.enc ++ 0b0100#4
  | .and    rd  rs1 rs2 =>          rs2.enc ++  rs1.enc ++  rd.enc ++ 0b0101#4
  | .or     rd  rs1 rs2 =>          rs2.enc ++  rs1.enc ++  rd.enc ++ 0b0110#4
  | .xor    rd  rs1 rs2 =>          rs2.enc ++  rs1.enc ++  rd.enc ++ 0b0111#4
  | rrc     rd  rs      =>         0b0000#4 ++   rs.enc ++  rd.enc ++ 0b1000#4
  | set     rd  imm     =>                          imm ++  rd.enc ++ 0b1001#4
  | .ld     rd  rsb imm =>              imm ++  rsb.enc ++  rd.enc ++ 0b1010#4
  | .st     rd  rsb imm =>              imm ++  rsb.enc ++  rd.enc ++ 0b1011#4
  | .rsr    rs1 rs2 csr =>          csr.enc ++  rs2.enc ++ rs1.enc ++ 0b1100#4
  | .wsr    rs1 rs2 csr =>          csr.enc ++  rs2.enc ++ rs1.enc ++ 0b1101#4
  | .jal    rs1 rs2 cmp => 0b0#1 ++ cmp.enc ++  rs2.enc ++ rs1.enc ++ 0b1110#4
  | .jmp    rs1 rs2 cmp => 0b0#1 ++ cmp.enc ++  rs2.enc ++ rs1.enc ++ 0b1111#4
  | .jmpCsr csr cmp     => 0b1#1 ++ cmp.enc ++ 0b0000#4 ++ csr.enc ++ 0b1111#4


--def Inst.dec : InstWord -> Option Inst  


end MisaSpec
