package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.LexCounterInterface.{LexCounterCompanion, State}
import rescala.extra.lattices.delta.interfaces.LexCounterInterface

class LexCounter(
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends LexCounterInterface[LexCounter] with ReactiveCRDT[State, LexCounter] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): LexCounter =
    new LexCounter(state, replicaID, deltaBuffer)
}

object LexCounter extends LexCounterCompanion {
  def apply(replicaID: String): LexCounter = new LexCounter(UIJDLattice[State].bottom, replicaID, List())
}
