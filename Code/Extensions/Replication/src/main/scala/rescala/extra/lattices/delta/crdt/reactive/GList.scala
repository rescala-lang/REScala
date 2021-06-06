package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.interfaces.GListInterface
import rescala.extra.lattices.delta.interfaces.GListInterface.{GListCompanion, State}
import rescala.extra.lattices.delta.{Delta, UIJDLattice}

class GList[E](
    protected[rescala] val state: State[E],
    protected val replicaID: String,
    protected[rescala] val deltaBuffer: List[Delta[State[E]]]
) extends GListInterface[E, GList[E]] with ReactiveCRDT[State[E], GList[E]] {

  override protected def copy(state: State[E], deltaBuffer: List[Delta[State[E]]]): GList[E] =
    new GList(state, replicaID, deltaBuffer)
}

object GList extends GListCompanion {
  def apply[E](replicaID: String): GList[E] = new GList(UIJDLattice[State[E]].bottom, replicaID, List())
}
