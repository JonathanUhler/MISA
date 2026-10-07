import MisaSpec.Basic


namespace MisaSpec


structure State where
  register : GpReg -> DataWord
  special  : CsrReg -> SpecWord
  memory   : AddrWord -> DataWord
  check    : CmpFlag -> Bool
  pc       : AddrWord
  halted   : Bool


end MisaSpec
