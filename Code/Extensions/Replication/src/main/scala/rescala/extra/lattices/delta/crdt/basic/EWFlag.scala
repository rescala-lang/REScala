package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.DotSet
import rescala.extra.lattices.delta.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import rescala.extra.lattices.delta.interfaces.EWFlagInterface
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

/** [[BasicCRDT Basic]] implementation of [[EWFlagInterface]]
  * @tparam C Type of the causal context used for this Causal CRDT
  */
class EWFlag[C: CContext](
    val state: State[C],
    protected val antiEntropy: AntiEntropy[State[C]]
) extends EWFlagInterface[C, EWFlag[C]] with BasicCRDT[State[C], EWFlag[C]] {

  override protected def copy(state: State[C]): EWFlag[C] = new EWFlag(state, antiEntropy)
}

object EWFlag extends EWFlagCompanion {

  /** Creates a new EWFlag instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam C Type of the causal context used for this Causal CRDT
    */
  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): EWFlag[C] =
    new EWFlag(UIJDLattice[State[C]].bottom, antiEntropy)
}
