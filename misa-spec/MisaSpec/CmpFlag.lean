import MisaSpec.Basic


namespace MisaSpec


inductive CmpFlag where
  | always | equal | notEqual | greater | less | greaterEqual | lessEqual
  deriving Repr


def CmpFlag.all : List CmpFlag :=
  [.always, .equal, .notEqual, .greater, .less, .greaterEqual, .lessEqual]


def CmpFlag.enc : CmpFlag -> CmpWord
  | .always       => 0b000
  | .equal        => 0b001
  | .notEqual     => 0b010
  | .greater      => 0b011
  | .less         => 0b100
  | .greaterEqual => 0b101
  | .lessEqual    => 0b110


def CmpFlag.dec (n : CmpWord) : Option CmpFlag :=
  CmpFlag.all.find? (·.enc == n)


end MisaSpec
