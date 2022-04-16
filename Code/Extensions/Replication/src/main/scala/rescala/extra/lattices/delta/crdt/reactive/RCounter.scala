package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.interfaces.RCounterInterface
import kofre.decompose.interfaces.RCounterInterface.{RCounterCompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.RCounterInterface RCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this causal CRDT
  */
class RCounter(
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends RCounterInterface[RCounter] with ReactiveCRDT[State, RCounter] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): RCounter =
    new RCounter(state, replicaID, deltaBuffer)
}

object RCounter extends RCounterCompanion {

  /** Creates a new RCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply(replicaID: String): RCounter =
    new RCounter(UIJDLattice[State].bottom, replicaID, List())
}
