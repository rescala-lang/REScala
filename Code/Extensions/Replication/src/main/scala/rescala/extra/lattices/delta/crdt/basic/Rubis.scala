package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.interfaces.RubisInterface
import rescala.extra.lattices.delta.interfaces.RubisInterface.{RubisCompanion, State}
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

/** [[BasicCRDT Basic]] implementation of [[RubisInterface]]
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
