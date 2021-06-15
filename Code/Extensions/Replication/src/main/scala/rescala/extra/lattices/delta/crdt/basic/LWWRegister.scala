package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.interfaces.LWWRegisterInterface
import rescala.extra.lattices.delta.interfaces.LWWRegisterInterface.{LWWCompanion, State}
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

class LWWRegister[A, C: CContext](
    val state: State[A, C],
    protected val antiEntropy: AntiEntropy[State[A, C]]
) extends LWWRegisterInterface[A, C, LWWRegister[A, C]] with BasicCRDT[State[A, C], LWWRegister[A, C]] {

  override protected def copy(state: State[A, C]): LWWRegister[A, C] = new LWWRegister(state, antiEntropy)
}

object LWWRegister extends LWWCompanion {
  def apply[A, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): LWWRegister[A, C] =
    new LWWRegister(UIJDLattice[State[A, C]].bottom, antiEntropy)
}
