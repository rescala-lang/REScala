package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.LWWRegisterInterface
import rescala.extra.lattices.delta.interfaces.LWWRegisterInterface.{LWWRegisterCompanion, State}

/** [[ReactiveCRDT Reactive]] implementation of [[LWWRegisterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class LWWRegister[A, C: CContext](
    val state: State[A, C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[A, C]]]
) extends LWWRegisterInterface[A, C, LWWRegister[A, C]] with ReactiveCRDT[State[A, C], LWWRegister[A, C]] {

  override protected def copy(state: State[A, C], deltaBuffer: List[Delta[State[A, C]]]): LWWRegister[A, C] =
    new LWWRegister(state, replicaID, deltaBuffer)
}

object LWWRegister extends LWWRegisterCompanion {

  /** Creates a new LWWRegister instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A, C: CContext](replicaID: String): LWWRegister[A, C] =
    new LWWRegister(UIJDLattice[State[A, C]].bottom, replicaID, List())
}
