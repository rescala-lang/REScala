package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.interfaces.GListInterface
import rescala.extra.lattices.delta.interfaces.GListInterface.{GListCompanion, State}
import rescala.extra.lattices.delta.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[GListInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
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
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam E Type of the elements in the list
    */
  def apply[E](replicaID: String): GList[E] = new GList(UIJDLattice[State[E]].bottom, replicaID, List())
}
