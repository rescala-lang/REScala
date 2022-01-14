package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.interfaces.RubisInterface
import kofre.decompose.interfaces.RubisInterface.{RubisCompanion, State}
import kofre.decompose.{CContext, UIJDLattice}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.RubisInterface RubisInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this causal CRDT
  */
class Rubis[C: CContext](
    val state: State[C],
    protected val antiEntropy: AntiEntropy[State[C]]
) extends RubisInterface[C, Rubis[C]] with BasicCRDT[State[C], Rubis[C]] {

  override protected def copy(state: State[C]): Rubis[C] = new Rubis(state, antiEntropy)
}

object Rubis extends RubisCompanion {

  /** Creates a new Rubis instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): Rubis[C] =
    new Rubis(UIJDLattice[State[C]].bottom, antiEntropy)
}
