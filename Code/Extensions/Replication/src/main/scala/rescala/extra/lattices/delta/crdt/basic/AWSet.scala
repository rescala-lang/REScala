package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.{DotMap, DotSet}
import kofre.decompose.interfaces.AWSetInterface
import kofre.decompose.interfaces.AWSetInterface.{AWSetCompanion, State}
import kofre.decompose.{UIJDLattice}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.AWSetInterface AWSetInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam E Type of the elements stored in the set
  * @tparam C Type of the causal context used for this causal CRDT
  */
class AWSet[E](
    val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends AWSetInterface[E, AWSet[E]] with BasicCRDT[State[E], AWSet[E]] {

  override protected def copy(state: State[E]): AWSet[E] = new AWSet(state, antiEntropy)
}

object AWSet extends AWSetCompanion {

  /** Creates a new AWSet instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam E Type of the elements stored in the set
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[E](antiEntropy: AntiEntropy[State[E]]): AWSet[E] =
    new AWSet(UIJDLattice[State[E]].bottom, antiEntropy)
}
