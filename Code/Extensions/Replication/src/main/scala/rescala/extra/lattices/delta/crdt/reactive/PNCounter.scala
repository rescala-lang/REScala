package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.PNCounterInterface
import rescala.extra.lattices.delta.interfaces.PNCounterInterface.{PNCounterCompanion, State}

class PNCounter(
    protected[rescala] val state: State,
    protected val replicaID: String,
    protected[rescala] val deltaBuffer: List[Delta[State]]
) extends PNCounterInterface[PNCounter] with ReactiveCRDT[State, PNCounter] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): PNCounter =
    new PNCounter(state, replicaID, deltaBuffer)
}

object PNCounter extends PNCounterCompanion {
  def apply(replicaID: String): PNCounter = new PNCounter(UIJDLattice[State].bottom, replicaID, List())
}
