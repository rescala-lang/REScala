package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.interfaces.MVRegisterInterface.{MVRegisterCompanion, State}
import rescala.extra.lattices.delta.interfaces.MVRegisterInterface
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}

class MVRegister[A: UIJDLattice, C: CContext](
    val state: State[A, C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[A, C]]]
) extends MVRegisterInterface[A, C, MVRegister[A, C]] with ReactiveCRDT[State[A, C], MVRegister[A, C]] {

  override protected def copy(state: State[A, C], deltaBuffer: List[Delta[State[A, C]]]): MVRegister[A, C] =
    new MVRegister(state, replicaID, deltaBuffer)
}

object MVRegister extends MVRegisterCompanion {
  def apply[A: UIJDLattice, C: CContext](replicaID: String): MVRegister[A, C] =
    new MVRegister(UIJDLattice[State[A, C]].bottom, replicaID, List())
}
