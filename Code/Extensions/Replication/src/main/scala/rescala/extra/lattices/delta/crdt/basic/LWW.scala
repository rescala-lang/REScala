package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.interfaces.LWWInterface
import rescala.extra.lattices.delta.interfaces.LWWInterface.{LWWCompanion, State}
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

class LWW[A, C: CContext](
    protected[rescala] val state: State[A, C],
    protected val antiEntropy: AntiEntropy[State[A, C]]
) extends LWWInterface[A, C, LWW[A, C]] with BasicCRDT[State[A, C], LWW[A, C]] {

  override protected def copy(state: State[A, C]): LWW[A, C] = new LWW(state, antiEntropy)
}

object LWW extends LWWCompanion {
  def apply[A, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): LWW[A, C] =
    new LWW(UIJDLattice[State[A, C]].bottom, antiEntropy)
}
