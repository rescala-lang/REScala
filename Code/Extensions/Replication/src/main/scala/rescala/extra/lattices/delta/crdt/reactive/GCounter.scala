package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.GCounterInterface.{GCounterCompanion, State}
import rescala.extra.lattices.delta.interfaces.GCounterInterface

class GCounter(
    protected[rescala] val state: State,
    protected val replicaID: String,
    protected[rescala] val deltaBuffer: List[Delta[State]]
) extends GCounterInterface[GCounter] with ReactiveCRDT[State, GCounter] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): GCounter =
    new GCounter(state, replicaID, deltaBuffer)
}

object GCounter extends GCounterCompanion {
  def apply(replicaID: String): GCounter = new GCounter(UIJDLattice[State].bottom, replicaID, List())
}
