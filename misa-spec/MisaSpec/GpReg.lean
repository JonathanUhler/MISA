import MisaSpec.Basic


namespace MisaSpec


inductive GpReg where
  | r0 | ra | rb | rc | rd | re | rf | ru | rv | rw | rx | ry | rz | rt
  | rscratch0 | rscratch1
  deriving Repr


def GpReg.all : List GpReg :=
  [.r0, .ra, .rb, .rc, .rd, .re, .rf, .ru, .rv, .rw, .rx, .ry, .rz, .rt, .rscratch0, .rscratch1]


def GpReg.enc : GpReg -> HalfWord
  | .r0        => 0b0000
  | .ra        => 0b0001
  | .rb        => 0b0010
  | .rc        => 0b0011
  | .rd        => 0b0100
  | .re        => 0b0101
  | .rf        => 0b0110
  | .ru        => 0b0111
  | .rv        => 0b1000
  | .rw        => 0b1001
  | .rx        => 0b1010
  | .ry        => 0b1011
  | .rz        => 0b1100
  | .rt        => 0b1101
  | .rscratch0 => 0b1110
  | .rscratch1 => 0b1111


def GpReg.dec (n : HalfWord) : Option GpReg :=
  GpReg.all.find? (·.enc == n)


end MisaSpec
