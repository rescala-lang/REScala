package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.interfaces.MVRegisterInterface.{MVRegisterCompanion, State}
import rescala.extra.lattices.delta.interfaces.MVRegisterInterface
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[MVRegisterInterface]]
  *
  * @tparam A Type of the stored value
  * @tparam C Type of the causal context used for this causal CRDT
  */
class MVRegister[A: UIJDLattice, C: CContext](
    val state: State[A, C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[A, C]]]
) extends MVRegisterInterface[A, C, MVRegister[A, C]] with ReactiveCRDT[State[A, C], MVRegister[A, C]] {

  override protected def copy(state: State[A, C], deltaBuffer: List[Delta[State[A, C]]]): MVRegister[A, C] =
    new MVRegister(state, replicaID, deltaBuffer)
}

object MVRegister extends MVRegisterCompanion {

  /** Creates a new MVRegister instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam A Type of the stored value
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[A: UIJDLattice, C: CContext](replicaID: String): MVRegister[A, C] =
    new MVRegister(UIJDLattice[State[A, C]].bottom, replicaID, List())
}
