package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.UIJDLattice
import kofre.decompose.interfaces.PNCounterInterface
import kofre.decompose.interfaces.PNCounterInterface.{PNCounterCompanion, State}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.PNCounterInterface PNCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class PNCounter(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends PNCounterInterface[PNCounter] with BasicCRDT[State, PNCounter] {

  override protected def copy(state: State): PNCounter = new PNCounter(state, antiEntropy)
}

object PNCounter extends PNCounterCompanion {

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply(antiEntropy: AntiEntropy[State]): PNCounter = new PNCounter(UIJDLattice[State].bottom, antiEntropy)
}
