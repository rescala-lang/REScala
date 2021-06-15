package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.interfaces.GListInterface
import rescala.extra.lattices.delta.interfaces.GListInterface.{GListCompanion, State}
import rescala.extra.lattices.delta.{Delta, UIJDLattice}

/** Reactive implementation of [[GListInterface]]
  * @tparam E Type of the elements in the list
  */
class GList[E](
    val state: State[E],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[E]]]
) extends GListInterface[E, GList[E]] with ReactiveCRDT[State[E], GList[E]] {

  override protected def copy(state: State[E], deltaBuffer: List[Delta[State[E]]]): GList[E] =
    new GList(state, replicaID, deltaBuffer)
}

object GList extends GListCompanion {

  /** Creates a new GList instance
    * @tparam E Type of the elements in the list
    */
  def apply[E](replicaID: String): GList[E] = new GList(UIJDLattice[State[E]].bottom, replicaID, List())
}
