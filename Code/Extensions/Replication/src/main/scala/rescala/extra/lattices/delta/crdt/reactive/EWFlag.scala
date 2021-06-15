package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotSet
import rescala.extra.lattices.delta.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import rescala.extra.lattices.delta.interfaces.EWFlagInterface
import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}

/** Reactive implementation of [[EWFlagInterface]]
  * @tparam C Type of the causal context used for this Causal CRDT
  */
class EWFlag[C: CContext](
    val state: State[C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[C]]]
) extends EWFlagInterface[C, EWFlag[C]] with ReactiveCRDT[State[C], EWFlag[C]] {

  override protected def copy(state: State[C], deltaBuffer: List[Delta[State[C]]]): EWFlag[C] =
    new EWFlag(state, replicaID, deltaBuffer)
}

object EWFlag extends EWFlagCompanion {

  /** Creates a new EWFlag instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam C Type of the causal context used for this Causal CRDT
    */
  def apply[C: CContext](replicaID: String): EWFlag[C] =
    new EWFlag(UIJDLattice[State[C]].bottom, replicaID, List())
}
