package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotSet
import rescala.extra.lattices.delta.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import rescala.extra.lattices.delta.interfaces.EWFlagInterface
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}

class EWFlag[C: CContext](
    protected[rescala] val state: State[C],
    protected val replicaID: String,
    protected[rescala] val deltaBuffer: List[Delta[State[C]]]
) extends EWFlagInterface[C, EWFlag[C]] with ReactiveCRDT[State[C], EWFlag[C]] {

  override protected def copy(state: State[C], deltaBuffer: List[Delta[State[C]]]): EWFlag[C] =
    new EWFlag(state, replicaID, deltaBuffer)
}

object EWFlag extends EWFlagCompanion {
  def apply[C: CContext](replicaID: String): EWFlag[C] =
    new EWFlag(UIJDLattice[State[C]].bottom, replicaID, List())
}
