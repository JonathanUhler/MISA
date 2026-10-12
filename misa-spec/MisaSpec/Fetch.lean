import MisaSpec.Basic
import MisaSpec.State


namespace MisaSpec


def fetch (q : State) : InstWord := q.memory (q.pc + 1) ++ q.memory q.pc


end MisaSpec
