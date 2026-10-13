import MisaSpec.Basic


namespace MisaSpec


inductive GpReg where
  | r0
  | ra
  | rb
  | rc
  | rd
  | re
  | rf
  | ru
  | rv
  | rw
  | rx
  | ry
  | rz
  | rt
  | rscratch0
  | rscratch1
  deriving Repr


def GpReg.all : List GpReg :=
  [
    .r0, .ra, .rb, .rc, .rd, .re, .rf, .ru, .rv, .rw, .rx, .ry, .rz, .rt, .rscratch0, .rscratch1
  ]


def GpReg.enc : GpReg -> HalfWord
  | .r0        => 0b0000#4
  | .ra        => 0b0001#4
  | .rb        => 0b0010#4
  | .rc        => 0b0011#4
  | .rd        => 0b0100#4
  | .re        => 0b0101#4
  | .rf        => 0b0110#4
  | .ru        => 0b0111#4
  | .rv        => 0b1000#4
  | .rw        => 0b1001#4
  | .rx        => 0b1010#4
  | .ry        => 0b1011#4
  | .rz        => 0b1100#4
  | .rt        => 0b1101#4
  | .rscratch0 => 0b1110#4
  | .rscratch1 => 0b1111#4


def GpReg.dec (n : HalfWord) : Option GpReg :=
  GpReg.all.find? (·.enc == n)


def GpReg.name : GpReg -> String
  | .r0 => "r0"
  | .ra => "ra"
  | .rb => "rb"
  | .rc => "rc"
  | .rd => "rd"
  | .re => "re"
  | .rf => "rf"
  | .ru => "ru"
  | .rv => "rv"
  | .rw => "rw"
  | .rx => "rx"
  | .ry => "ry"
  | .rz => "rz"
  | .rt => "rt"
  | .rscratch0 => "rscratch0"
  | .rscratch1 => "rscratch1"


end MisaSpec
