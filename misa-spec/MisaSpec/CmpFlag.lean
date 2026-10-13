import MisaSpec.Basic


namespace MisaSpec


inductive CmpFlag where
  | always
  | equal
  | notEqual
  | greater
  | less
  | greaterEqual
  | lessEqual
  deriving DecidableEq, Repr


def CmpFlag.all : List CmpFlag :=
  [
    .always, .equal, .notEqual, .greater, .less, .greaterEqual, .lessEqual
  ]


def CmpFlag.enc : CmpFlag -> CmpWord
  | .always       => 0b000#3
  | .equal        => 0b001#3
  | .notEqual     => 0b010#3
  | .greater      => 0b011#3
  | .less         => 0b100#3
  | .greaterEqual => 0b101#3
  | .lessEqual    => 0b110#3


def CmpFlag.dec (n : CmpWord) : Option CmpFlag :=
  CmpFlag.all.find? (·.enc == n)


end MisaSpec
