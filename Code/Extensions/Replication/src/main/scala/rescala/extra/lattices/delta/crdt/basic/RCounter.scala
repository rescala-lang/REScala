package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.{CContext, UIJDLattice}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.interfaces.RCounterInterface
import rescala.extra.lattices.delta.interfaces.RCounterInterface.{RCounterCompanion, State}

class RCounter[C: CContext](
    val state: State[C],
    protected val antiEntropy: AntiEntropy[State[C]]
) extends RCounterInterface[C, RCounter[C]] with BasicCRDT[State[C], RCounter[C]] {

  override protected def copy(state: State[C]): RCounter[C] = new RCounter(state, antiEntropy)
}

object RCounter extends RCounterCompanion {
  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): RCounter[C] =
    new RCounter(UIJDLattice[State[C]].bottom, antiEntropy)
}
