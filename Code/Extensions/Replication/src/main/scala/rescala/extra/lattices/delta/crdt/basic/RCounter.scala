package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.{CContext, UIJDLattice}
import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.RCounterInterface
import kofre.decompose.interfaces.RCounterInterface.{RCounterCompanion, State}

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.RCounterInterface RCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this causal CRDT
  */
class RCounter[C: CContext](
    val state: State[C],
    protected val antiEntropy: AntiEntropy[State[C]]
) extends RCounterInterface[C, RCounter[C]] with BasicCRDT[State[C], RCounter[C]] {

  override protected def copy(state: State[C]): RCounter[C] = new RCounter(state, antiEntropy)
}

object RCounter extends RCounterCompanion {

  /** Creates a new RCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): RCounter[C] =
    new RCounter(UIJDLattice[State[C]].bottom, antiEntropy)
}
