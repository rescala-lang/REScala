package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.interfaces.RGAInterface
import rescala.extra.lattices.delta.interfaces.RGAInterface.{RGACompanion, State}
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}

class RGA[E, C: CContext](
    protected[rescala] val state: State[E, C],
    protected val replicaID: String,
    protected[rescala] val deltaBuffer: List[Delta[State[E, C]]]
) extends RGAInterface[E, C, RGA[E, C]] with ReactiveCRDT[State[E, C], RGA[E, C]] {

  override protected def copy(state: State[E, C], deltaBuffer: List[Delta[State[E, C]]]): RGA[E, C] =
    new RGA(state, replicaID, deltaBuffer)
}

object RGA extends RGACompanion {
  def apply[E, C: CContext](replicaID: String): RGA[E, C] =
    new RGA(UIJDLattice[State[E, C]].bottom, replicaID, List())
}
