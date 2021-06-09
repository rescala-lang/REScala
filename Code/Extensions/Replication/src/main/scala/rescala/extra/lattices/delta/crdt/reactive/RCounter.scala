package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.RCounterInterface
import rescala.extra.lattices.delta.interfaces.RCounterInterface.{RCounterCompanion, State}

class RCounter[C: CContext](
    val state: State[C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[C]]]
) extends RCounterInterface[C, RCounter[C]] with ReactiveCRDT[State[C], RCounter[C]] {

  override protected def copy(state: State[C], deltaBuffer: List[Delta[State[C]]]): RCounter[C] =
    new RCounter(state, replicaID, deltaBuffer)
}

object RCounter extends RCounterCompanion {
  def apply[C: CContext](replicaID: String): RCounter[C] =
    new RCounter(UIJDLattice[State[C]].bottom, replicaID, List())
}
