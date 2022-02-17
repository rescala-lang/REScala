package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.DotStore.DotSet
import kofre.decompose.interfaces.EWFlagInterface
import kofre.decompose.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import kofre.decompose.{UIJDLattice}

import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.EWFlagInterface EWFlagInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam C Type of the causal context used for this Causal CRDT
  */
class EWFlag(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends EWFlagInterface[EWFlag] with BasicCRDT[State, EWFlag] {

  override protected def copy(state: State): EWFlag = new EWFlag(state, antiEntropy)
}

object EWFlag extends EWFlagCompanion {

  /** Creates a new EWFlag instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam C Type of the causal context used for this Causal CRDT
    */
  def apply(antiEntropy: AntiEntropy[State]): EWFlag =
    new EWFlag(UIJDLattice[State].bottom, antiEntropy)
}
