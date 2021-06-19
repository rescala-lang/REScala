package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.{DotMap, DotSet}
import rescala.extra.lattices.delta.interfaces.AWSetInterface.{AWSetCompanion, State}
import rescala.extra.lattices.delta.interfaces.AWSetInterface
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[AWSetInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam E Type of the elements stored in the set
  * @tparam C Type of the causal context used for this causal CRDT
  */
class AWSet[E, C: CContext](
    val state: State[E, C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[E, C]]]
) extends AWSetInterface[E, C, AWSet[E, C]] with ReactiveCRDT[State[E, C], AWSet[E, C]] {

  override protected def copy(state: State[E, C], deltaBuffer: List[Delta[State[E, C]]]): AWSet[E, C] =
    new AWSet(state, replicaID, deltaBuffer)
}

object AWSet extends AWSetCompanion {

  /** Creates a new AWSet instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam E Type of the elements stored in the set
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[E, C: CContext](replicaID: String): AWSet[E, C] =
    new AWSet(UIJDLattice[State[E, C]].bottom, replicaID, List())
}
