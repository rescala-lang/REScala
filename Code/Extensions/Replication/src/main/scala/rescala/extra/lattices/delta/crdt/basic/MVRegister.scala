package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.MVRegisterInterface
import kofre.decompose.interfaces.MVRegisterInterface.{MVRegisterCompanion, State}
import kofre.decompose.{UIJDLattice}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.MVRegisterInterface MVRegisterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class MVRegister[A: UIJDLattice](
    val state: State[A],
    protected val antiEntropy: AntiEntropy[State[A]]
) extends MVRegisterInterface[A, MVRegister[A]] with BasicCRDT[State[A], MVRegister[A]] {

  override protected def copy(state: State[A]): MVRegister[A] = new MVRegister(state, antiEntropy)
}

object MVRegister extends MVRegisterCompanion {

  /** Creates a new MVRegister instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A: UIJDLattice](antiEntropy: AntiEntropy[State[A]]): MVRegister[A] =
    new MVRegister(UIJDLattice[State[A]].bottom, antiEntropy)
}
