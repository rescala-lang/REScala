package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.MVRegisterInterface
import kofre.decompose.interfaces.MVRegisterInterface.{MVRegisterCompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.MVRegisterInterface MVRegisterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class MVRegister[A: UIJDLattice](
    val state: State[A],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[A]]]
) extends MVRegisterInterface[A, MVRegister[A]] with ReactiveCRDT[State[A], MVRegister[A]] {

  override protected def copy(state: State[A], deltaBuffer: List[Delta[State[A]]]): MVRegister[A] =
    new MVRegister(state, replicaID, deltaBuffer)
}

object MVRegister extends MVRegisterCompanion {

  /** Creates a new MVRegister instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A: UIJDLattice](replicaID: String): MVRegister[A] =
    new MVRegister(UIJDLattice[State[A]].bottom, replicaID, List())
}
