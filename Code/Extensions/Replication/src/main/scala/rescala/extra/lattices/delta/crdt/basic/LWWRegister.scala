package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.LWWRegisterInterface
import kofre.decompose.interfaces.LWWRegisterInterface.{LWWRegisterCompanion, State}
import kofre.decompose.{CContext, UIJDLattice}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.LWWRegisterInterface LWWRegisterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class LWWRegister[A, C: CContext](
    val state: State[A, C],
    protected val antiEntropy: AntiEntropy[State[A, C]]
) extends LWWRegisterInterface[A, C, LWWRegister[A, C]] with BasicCRDT[State[A, C], LWWRegister[A, C]] {

  override protected def copy(state: State[A, C]): LWWRegister[A, C] = new LWWRegister(state, antiEntropy)
}

object LWWRegister extends LWWRegisterCompanion {

  /** Creates a new LWWRegister instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A, C: CContext](antiEntropy: AntiEntropy[State[A, C]]): LWWRegister[A, C] =
    new LWWRegister(UIJDLattice[State[A, C]].bottom, antiEntropy)
}
