package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.interfaces.PNCounterModule.PNCounter
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.PNCounterInterface PNCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class ReactivePNCounter(
    val state: PNCounter,
    val replicaID: String,
    val deltaBuffer: List[Delta[PNCounter]]
) extends ReactiveCRDT[PNCounter, ReactivePNCounter] {

  override protected def copy(state: PNCounter, deltaBuffer: List[Delta[PNCounter]]): ReactivePNCounter =
    new ReactivePNCounter(state, replicaID, deltaBuffer)
}

object ReactivePNCounter {

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply(replicaID: String): ReactivePNCounter =
    new ReactivePNCounter(UIJDLattice[PNCounter].bottom, replicaID, List())
}
