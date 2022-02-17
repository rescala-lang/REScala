package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.LWWRegisterInterface
import kofre.decompose.interfaces.LWWRegisterInterface.{LWWRegisterCompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.LWWRegisterInterface LWWRegisterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class LWWRegister[A](
    val state: State[A],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[A]]]
) extends LWWRegisterInterface[A, LWWRegister[A]] with ReactiveCRDT[State[A], LWWRegister[A]] {

  override protected def copy(state: State[A], deltaBuffer: List[Delta[State[A]]]): LWWRegister[A] =
    new LWWRegister(state, replicaID, deltaBuffer)
}

object LWWRegister extends LWWRegisterCompanion {

  /** Creates a new LWWRegister instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A](replicaID: String): LWWRegister[A] =
    new LWWRegister(UIJDLattice[State[A]].bottom, replicaID, List())
}
