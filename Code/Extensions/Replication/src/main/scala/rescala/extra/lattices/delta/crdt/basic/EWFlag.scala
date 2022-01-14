package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.DotSet
import kofre.decompose.interfaces.EWFlagInterface
import kofre.decompose.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import kofre.decompose.{CContext, UIJDLattice}

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.EWFlagInterface EWFlagInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
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
