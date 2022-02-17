package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.DotFun
import kofre.decompose.interfaces.RCounterInterface
import kofre.decompose.interfaces.RCounterInterface.{RCounterCompanion, State}
import kofre.decompose.{UIJDLattice}

import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.RCounterInterface RCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this causal CRDT
  */
class RCounter(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends RCounterInterface[RCounter] with BasicCRDT[State, RCounter] {

  override protected def copy(state: State): RCounter = new RCounter(state, antiEntropy)
}

object RCounter extends RCounterCompanion {

  /** Creates a new RCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply(antiEntropy: AntiEntropy[State]): RCounter =
    new RCounter(UIJDLattice[State].bottom, antiEntropy)
}
