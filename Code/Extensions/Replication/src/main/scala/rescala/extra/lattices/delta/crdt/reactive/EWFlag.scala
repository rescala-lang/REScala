package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.DotStore.DotSet
import kofre.decompose.interfaces.EWFlagInterface
import kofre.decompose.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.EWFlagInterface EWFlagInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this Causal CRDT
  */
class EWFlag(
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends EWFlagInterface[EWFlag] with ReactiveCRDT[State, EWFlag] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): EWFlag =
    new EWFlag(state, replicaID, deltaBuffer)
}

object EWFlag extends EWFlagCompanion {

  /** Creates a new EWFlag instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam C Type of the causal context used for this Causal CRDT
    */
  def apply(replicaID: String): EWFlag =
    new EWFlag(UIJDLattice[State].bottom, replicaID, List())
}
