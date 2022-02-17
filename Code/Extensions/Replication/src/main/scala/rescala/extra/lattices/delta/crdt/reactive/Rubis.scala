package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.interfaces.RubisInterface
import kofre.decompose.interfaces.RubisInterface.{RubisCompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.RubisInterface RubisInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this causal CRDT
  */
class Rubis(
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends RubisInterface[Rubis] with ReactiveCRDT[State, Rubis] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): Rubis =
    new Rubis(state, replicaID, deltaBuffer)
}

object Rubis extends RubisCompanion {

  /** Creates a new Rubis instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply(replicaID: String): Rubis =
    new Rubis(UIJDLattice[State].bottom, replicaID, List())
}
