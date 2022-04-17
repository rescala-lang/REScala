package rescala.extra.lattices.delta.crdt.basic

import kofre.protocol.RubisInterface.{RubisCompanion, State}
import kofre.decompose.UIJDLattice
import kofre.protocol.RubisInterface

import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.RubisInterface RubisInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this causal CRDT
  */
class Rubis(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends RubisInterface[Rubis] with BasicCRDT[State, Rubis] {

  override protected def copy(state: State): Rubis = new Rubis(state, antiEntropy)
}

object Rubis extends RubisCompanion {

  /** Creates a new Rubis instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply(antiEntropy: AntiEntropy[State]): Rubis =
    new Rubis(UIJDLattice[State].bottom, antiEntropy)
}
