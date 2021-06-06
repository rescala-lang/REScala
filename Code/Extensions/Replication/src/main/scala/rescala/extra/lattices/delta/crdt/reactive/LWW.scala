package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.LWWInterface
import rescala.extra.lattices.delta.interfaces.LWWInterface.{LWWCompanion, State}

class LWW[A, C: CContext](
    protected[rescala] val state: State[A, C],
    protected val replicaID: String,
    protected[rescala] val deltaBuffer: List[Delta[State[A, C]]]
) extends LWWInterface[A, C, LWW[A, C]] with ReactiveCRDT[State[A, C], LWW[A, C]] {

  override protected def copy(state: State[A, C], deltaBuffer: List[Delta[State[A, C]]]): LWW[A, C] =
    new LWW(state, replicaID, deltaBuffer)
}

object LWW extends LWWCompanion {
  def apply[A, C: CContext](replicaID: String): LWW[A, C] =
    new LWW(UIJDLattice[State[A, C]].bottom, replicaID, List())
}
