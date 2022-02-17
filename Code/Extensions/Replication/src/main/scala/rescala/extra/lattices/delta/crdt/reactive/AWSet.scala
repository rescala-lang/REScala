package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.DotStore.{DotMap, DotSet}
import kofre.decompose.interfaces.AWSetInterface
import kofre.decompose.interfaces.AWSetInterface.{AWSetCompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.AWSetInterface AWSetInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam E Type of the elements stored in the set
  * @tparam C Type of the causal context used for this causal CRDT
  */
class AWSet[E](
    val state: State[E],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[E]]]
) extends AWSetInterface[E,AWSet[E]] with ReactiveCRDT[State[E], AWSet[E]] {

  override protected def copy(state: State[E], deltaBuffer: List[Delta[State[E]]]): AWSet[E] =
    new AWSet(state, replicaID, deltaBuffer)
}

object AWSet extends AWSetCompanion {

  /** Creates a new AWSet instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam E Type of the elements stored in the set
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[E](replicaID: String): AWSet[E] =
    new AWSet(UIJDLattice[State[E]].bottom, replicaID, List())
}
