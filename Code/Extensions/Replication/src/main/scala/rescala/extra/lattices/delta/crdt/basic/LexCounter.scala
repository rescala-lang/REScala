package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.UIJDLattice
import kofre.decompose.interfaces.LexCounterInterface
import kofre.decompose.interfaces.LexCounterInterface.{LexCounterCompanion, State}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.LexCounterInterface LexCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class LexCounter(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends LexCounterInterface[LexCounter] with BasicCRDT[State, LexCounter] {

  override protected def copy(state: State = state): LexCounter = new LexCounter(state, antiEntropy)
}

object LexCounter extends LexCounterCompanion {

  /** Creates a new LexCounter instance
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply(antiEntropy: AntiEntropy[State]): LexCounter = new LexCounter(UIJDLattice[State].bottom, antiEntropy)
}
