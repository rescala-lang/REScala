package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.UIJDLattice
import kofre.decompose.interfaces.PNCounterModule.PNCounter
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.PNCounterInterface PNCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class BasicPNCounter(
    val state: PNCounter,
    protected val antiEntropy: AntiEntropy[PNCounter]
) extends BasicCRDT[PNCounter, BasicPNCounter] {

  override protected def copy(state: PNCounter): BasicPNCounter = new BasicPNCounter(state, antiEntropy)
}

object BasicPNCounter {

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply(antiEntropy: AntiEntropy[PNCounter]): BasicPNCounter = new BasicPNCounter(UIJDLattice[PNCounter].bottom, antiEntropy)
}
