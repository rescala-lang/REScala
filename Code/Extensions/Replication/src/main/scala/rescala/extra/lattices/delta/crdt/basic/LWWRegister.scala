package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.LWWRegisterInterface
import kofre.decompose.interfaces.LWWRegisterInterface.{LWWRegisterCompanion, State}
import kofre.decompose.{UIJDLattice}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.LWWRegisterInterface LWWRegisterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class LWWRegister[A](
    val state: State[A],
    protected val antiEntropy: AntiEntropy[State[A]]
) extends LWWRegisterInterface[A, LWWRegister[A]] with BasicCRDT[State[A], LWWRegister[A]] {

  override protected def copy(state: State[A]): LWWRegister[A] = new LWWRegister(state, antiEntropy)
}

object LWWRegister extends LWWRegisterCompanion {

  /** Creates a new LWWRegister instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A](antiEntropy: AntiEntropy[State[A]]): LWWRegister[A] =
    new LWWRegister(UIJDLattice[State[A]].bottom, antiEntropy)
}
