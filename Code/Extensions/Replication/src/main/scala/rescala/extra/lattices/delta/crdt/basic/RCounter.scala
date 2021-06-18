package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.{CContext, UIJDLattice}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.interfaces.RCounterInterface
import rescala.extra.lattices.delta.interfaces.RCounterInterface.{RCounterCompanion, State}

/** [[BasicCRDT Basic]] implementation of [[RCounterInterface]]
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
