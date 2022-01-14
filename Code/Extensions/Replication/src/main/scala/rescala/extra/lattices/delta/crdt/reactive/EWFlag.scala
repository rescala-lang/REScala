package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.DotStore.DotSet
import kofre.decompose.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import kofre.decompose.interfaces.EWFlagInterface
import kofre.decompose.{CContext, Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.EWFlagInterface EWFlagInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
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
