package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.{DotMap, DotSet}
import rescala.extra.lattices.delta.interfaces.AWSetInterface.{AWSetCompanion, State}
import rescala.extra.lattices.delta.interfaces.AWSetInterface
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

/** [[BasicCRDT Basic]] implementation of [[AWSetInterface]]
  *
  * @tparam E Type of the elements stored in the set
  * @tparam C Type of the causal context used for this causal CRDT
  */
class AWSet[E, C: CContext](
    val state: State[E, C],
    protected val antiEntropy: AntiEntropy[State[E, C]]
) extends AWSetInterface[E, C, AWSet[E, C]] with BasicCRDT[State[E, C], AWSet[E, C]] {

  override protected def copy(state: State[E, C]): AWSet[E, C] = new AWSet(state, antiEntropy)
}

object AWSet extends AWSetCompanion {

  /** Creates a new AWSet instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam E Type of the elements stored in the set
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): AWSet[E, C] =
    new AWSet(UIJDLattice[State[E, C]].bottom, antiEntropy)
}
