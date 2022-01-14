package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.MVRegisterInterface.{MVRegisterCompanion, State}
import kofre.decompose.interfaces.MVRegisterInterface
import kofre.decompose.{CContext, UIJDLattice}

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.MVRegisterInterface MVRegisterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class MVRegister[A: UIJDLattice, C: CContext](
    val state: State[A, C],
    protected val antiEntropy: AntiEntropy[State[A, C]]
) extends MVRegisterInterface[A, C, MVRegister[A, C]] with BasicCRDT[State[A, C], MVRegister[A, C]] {

  override protected def copy(state: State[A, C]): MVRegister[A, C] = new MVRegister(state, antiEntropy)
}

object MVRegister extends MVRegisterCompanion {

  /** Creates a new MVRegister instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A: UIJDLattice, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): MVRegister[A, C] =
    new MVRegister(UIJDLattice[State[A, C]].bottom, antiEntropy)
}
