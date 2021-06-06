package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.interfaces.MVRegisterInterface.{MVRegisterCompanion, State}
import rescala.extra.lattices.delta.interfaces.MVRegisterInterface
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

class MVRegister[A: UIJDLattice, C: CContext](
    protected[rescala] val state: State[A, C],
    protected val antiEntropy: AntiEntropy[State[A, C]]
) extends MVRegisterInterface[A, C, MVRegister[A, C]] with BasicCRDT[State[A, C], MVRegister[A, C]] {

  override protected def copy(state: State[A, C]): MVRegister[A, C] = new MVRegister(state, antiEntropy)
}

object MVRegister extends MVRegisterCompanion {
  def apply[A: UIJDLattice, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): MVRegister[A, C] =
    new MVRegister(UIJDLattice[State[A, C]].bottom, antiEntropy)
}
