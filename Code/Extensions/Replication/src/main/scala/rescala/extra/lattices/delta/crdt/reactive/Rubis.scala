package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{CContext, Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.RubisInterface
import rescala.extra.lattices.delta.interfaces.RubisInterface.{RubisCompanion, State}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.RubisInterface RubisInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this causal CRDT
  */
class Rubis[C: CContext](
    val state: State[C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[C]]]
) extends RubisInterface[C, Rubis[C]] with ReactiveCRDT[State[C], Rubis[C]] {

  override protected def copy(state: State[C], deltaBuffer: List[Delta[State[C]]]): Rubis[C] =
    new Rubis(state, replicaID, deltaBuffer)
}

object Rubis extends RubisCompanion {

  /** Creates a new Rubis instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[C: CContext](replicaID: String): Rubis[C] =
    new Rubis(UIJDLattice[State[C]].bottom, replicaID, List())
}
