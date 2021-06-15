package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.TwoPSetInterface
import rescala.extra.lattices.delta.interfaces.TwoPSetInterface.{State, TwoPSetCompanion}

/** Reactive implementation of [[TwoPSetInterface]]
  *
  * @tparam E Type of the elements stored in the set
  */
class TwoPSet[E](
    val state: State[E],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[E]]]
) extends TwoPSetInterface[E, TwoPSet[E]] with ReactiveCRDT[State[E], TwoPSet[E]] {

  override protected def copy(state: (Set[E], Set[E]), deltaBuffer: List[Delta[State[E]]]): TwoPSet[E] =
    new TwoPSet(state, replicaID, deltaBuffer)
}

object TwoPSet extends TwoPSetCompanion {

  /** Creates a new TwoPSet instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam E Type of the elements stored in the set
    */
  def apply[E](replicaID: String): TwoPSet[E] = new TwoPSet(UIJDLattice[State[E]].bottom, replicaID, List())
}
