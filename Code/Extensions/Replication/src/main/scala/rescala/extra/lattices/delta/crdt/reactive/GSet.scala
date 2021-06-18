package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.GSetInterface.{GSetCompanion, State}
import rescala.extra.lattices.delta.interfaces.GSetInterface

/** [[ReactiveCRDT Reactive]] implementation of [[GSetInterface]]
  * @tparam E Type of the elements stored in the set
  */
class GSet[E](
    val state: State[E],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[E]]]
) extends GSetInterface[E, GSet[E]] with ReactiveCRDT[State[E], GSet[E]] {

  override protected def copy(state: State[E], deltaBuffer: List[Delta[State[E]]]): GSet[E] =
    new GSet(state, replicaID, deltaBuffer)
}

object GSet extends GSetCompanion {

  /** Creates a new GSet instance
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam E Type of the elements stored in the set
    */
  def apply[E](replicaID: String): GSet[E] = new GSet(UIJDLattice[State[E]].bottom, replicaID, List())
}
