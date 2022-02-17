package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.{DotFun, DotPair}
import kofre.decompose.interfaces.RGAInterface
import kofre.decompose.interfaces.RGAInterface.{RGACompanion, State}
import kofre.decompose.{UIJDLattice}

import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.RGAInterface RGAInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam E Type of the elements stored in the list
  * @tparam C Type of the causal context used for this causal CRDT
  */
class RGA[E](
    val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends RGAInterface[E, RGA[E]] with BasicCRDT[State[E], RGA[E]] {

  override protected def copy(state: State[E]): RGA[E] = new RGA(state, antiEntropy)
}

object RGA extends RGACompanion {

  /** Creates a new RGA instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam E Type of the elements stored in the list
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[E](antiEntropy: AntiEntropy[State[E]]): RGA[E] =
    new RGA(UIJDLattice[State[E]].bottom, antiEntropy)
}
