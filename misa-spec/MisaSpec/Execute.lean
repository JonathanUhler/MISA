import MisaSpec.Basic
import MisaSpec.CsrReg
import MisaSpec.CmpFlag
import MisaSpec.Fetch
import MisaSpec.GpReg
import MisaSpec.Inst
import MisaSpec.State


namespace MisaSpec


def aluAdd (a : DataWord) (b : DataWord) (cin : Bool) : DataWord × CsrFlags :=
  let wide  : BitVec 9 := a.setWidth 9 + b.setWidth 9 + (if cin then 1 else 0 : BitVec 9)
  let res   : DataWord := wide.setWidth 8
  let flags : CsrFlags :=
    { z := res == 0,
      c := wide.getLsbD 8,
      n := res.getLsbD 7,
      v := (a.getLsbD 7 == b.getLsbD 7) && (b.getLsbD 7 != res.getLsbD 7) }
  (res, flags)


def executeHalt (q : State) (rs : GpReg) : State :=
  if q.userMode then
    q.fault CsrCause.privilegedInstruction
  else
    let cause : CsrCause := CsrCause.haltInstruction (q.register rs)
    { (q.writeSpecial .cause cause.enc) with halted := true }


def executeSyscall (q : State) (rs : GpReg) : State :=
  if q.inSyscall || q.inInterrupt then
    q.fault (CsrCause.improperSystemCall q.inSyscall q.inInterrupt)
  else
    let cause : CsrCause := CsrCause.haltInstruction (q.register rs)
    let target : AddrWord := q.memory 0xFFFB ++ q.memory 0xFFFA
    let saved : SpecWord := q.special .privs
    let q := (q.writeSpecial .cause cause.enc).writeSpecial .retsc (q.pc + 2)
    { (q.writeSpecial .privs 0x0000)
      with pc := target, inSyscall := true, syscallPrivs := saved }


def executeAdd (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) : State :=
  let (res, flags) :=
    aluAdd (q.register rs1) (q.register rs2) false
  ((q.writeReg rd res).writeFlags flags).next


def executeAdc (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) : State :=
  let (res, flags) :=
    aluAdd (q.register rs1) (q.register rs2) (CsrFlags.dec (q.special .flags)).c
  ((q.writeReg rd res).writeFlags flags).next


def executeSub (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) : State :=
  let (res, flags) :=
    aluAdd (q.register rs1) (~~~(q.register rs2)) true
  ((q.writeReg rd res).writeFlags flags).next


def executeSbb (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) : State :=
  let (res, flags) :=
    aluAdd (q.register rs1) (~~~(q.register rs2)) (CsrFlags.dec (q.special .flags)).c
  ((q.writeReg rd res).writeFlags flags).next


def executeAnd (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) : State :=
  let res   := q.register rs1 &&& q.register rs2
  let flags := { z := res == 0, c := false, n := res.getLsbD 7, v := false }
  ((q.writeReg rd res).writeFlags flags).next


def executeOr (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) : State :=
  let res   := q.register rs1 ||| q.register rs2
  let flags := { z := res == 0, c := false, n := res.getLsbD 7, v := false }
  ((q.writeReg rd res).writeFlags flags).next


def executeXor (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) : State :=
  let res   := q.register rs1 ^^^ q.register rs2
  let flags := { z := res == 0, c := false, n := res.getLsbD 7, v := false }
  ((q.writeReg rd res).writeFlags flags).next


def executeRrc (q : State) (rd : GpReg) (rs : GpReg) : State :=
  let a     := q.register rs
  let cin   := (CsrFlags.dec (q.special .flags)).c
  let res   := (if cin then (0x80 : DataWord) else 0) ||| (a >>> 1)
  let flags := { z := res == 0, c := a.getLsbD 0, n := res.getLsbD 7, v := false }
  ((q.writeReg rd res).writeFlags flags).next


def executeSet (q : State) (rd : GpReg) (imm : DataWord) : State :=
  (q.writeReg rd imm).next


def executeLd (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord) : State :=
  match rsb with
  | .rscratch1 =>
    let saddr' : AddrWord := q.special .saddr + imm.signExtend 16
    let q := q.writeSpecial .saddr saddr'
    (q.writeReg rd (q.memory saddr')).next
  | _ =>
    let lo := (GpReg.dec (rsb.enc + 1)).getD .r0
    let addr : AddrWord := (q.register rsb ++ q.register lo) + imm.signExtend 16
    (q.writeReg rd (q.memory addr)).next


def executeSt (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord) : State :=
  match rsb with
  | .rscratch1 =>
    let saddr : AddrWord := q.special .saddr
    let q := q.writeMem saddr (q.register rd)
    (q.writeSpecial .saddr (saddr + imm.signExtend 16)).next
  | _ =>
    let lo := (GpReg.dec (rsb.enc + 1)).getD .r0
    let addr : AddrWord := (q.register rsb ++ q.register lo) + imm.signExtend 16
    (q.writeMem addr (q.register rd)).next


def executeRsr (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg) : State :=
  if q.userMode && !csr.userReadable then
    q.fault CsrCause.privilegedInstruction
  else
    let v := q.special csr
    ((q.writeReg rs1 (v.extractLsb' 8 8)).writeReg rs2 (v.extractLsb' 0 8)).next


def executeWsr (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg) : State :=
  if q.userMode && !csr.userWritable then
    q.fault CsrCause.privilegedInstruction
  else
    (q.writeSpecial csr (q.register rs1 ++ q.register rs2)).next


def executeJal (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag) : State :=
  if q.check cmp then
    let target : AddrWord := q.register rs1 ++ q.register rs2
    { (q.writeSpecial .raddr (q.pc + 2)) with pc := target }
  else
    q.next


def executeJmp (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag) : State :=
  if q.check cmp then
    { q with pc := q.register rs1 ++ q.register rs2 }
  else
    q.next


def executeJmpCsr (q : State) (csr : CsrReg) (cmp : CmpFlag) : State :=
  if q.check cmp then
    match csr with
    | .retsc =>
      if cmp == .always && q.inSyscall && !q.inInterrupt then
        { (q.writeSpecial .privs q.syscallPrivs)
          with pc := q.special csr, inSyscall := false }
      else if cmp == .always then
        q.fault (CsrCause.improperReturnContext q.inSyscall q.inInterrupt csr)
      else
        q.fault (CsrCause.conditionalContextReturn cmp csr)
    -- Interrupt extension
    | .retir =>
      if cmp == .always && q.inInterrupt then
        let flags : CsrFlags := CsrFlags.dec q.intFlags
        let q := ((q.writeReg .rscratch0 (q.intRscratch.extractLsb' 8 8)).writeReg
            .rscratch1 (q.intRscratch.extractLsb' 0 8)).writeFlags flags
        { (q.writeSpecial .privs q.intPrivs)
          with pc := q.special csr, inInterrupt := false }
      else if cmp == .always then
        q.fault (CsrCause.improperReturnContext q.inSyscall q.inInterrupt csr)
      else
        q.fault (CsrCause.conditionalContextReturn cmp csr)
    | _      => { q with pc := q.special csr }
  else
    q.next


def execute (q : State) : State :=
  let extns : CsrExtns := CsrExtns.dec (q.special .extns)
  match Inst.decUnder (fetch q) extns with
  | none      => q.fault CsrCause.illegalInstruction
  | some inst =>
    match inst with
    | .halt    rs          => executeHalt   q rs
    | .add     rd  rs1 rs2 => executeAdd    q rd  rs1 rs2
    | .adc     rd  rs1 rs2 => executeAdc    q rd  rs1 rs2
    | .sub     rd  rs1 rs2 => executeSub    q rd  rs1 rs2
    | .sbb     rd  rs1 rs2 => executeSbb    q rd  rs1 rs2
    | .and     rd  rs1 rs2 => executeAnd    q rd  rs1 rs2
    | .or      rd  rs1 rs2 => executeOr     q rd  rs1 rs2
    | .xor     rd  rs1 rs2 => executeXor    q rd  rs1 rs2
    | .rrc     rd  rs      => executeRrc    q rd  rs
    | .set     rd  imm     => executeSet    q rd  imm
    | .ld      rd  rsb imm => executeLd     q rd  rsb imm
    | .st      rd  rsb imm => executeSt     q rd  rsb imm
    | .rsr     rs1 rs2 csr => executeRsr    q rs1 rs2 csr
    | .wsr     rs1 rs2 csr => executeWsr    q rs1 rs2 csr
    | .jal     rs1 rs2 cmp => executeJal    q rs1 rs2 cmp
    | .jmp     rs1 rs2 cmp => executeJmp    q rs1 rs2 cmp
    | .jmpCsr  csr cmp     => executeJmpCsr q csr cmp
    -- System call extension
    | .syscall rs          => if extns.syscall then executeSyscall q rs else executeHalt q rs


end MisaSpec
