import MisaSpec.Basic
import MisaSpec.State


namespace MisaSpec


def fetch (q : State) : Except CsrCause InstWord :=
  match q.accessFault .execute q.pc with
  | some cause => .error cause
  | none       =>
    match q.accessFault .execute (q.pc + 1) with
    | some cause => .error cause
    | none       => .ok (q.memory (q.pc + 1) ++ q.memory q.pc)


end MisaSpec
