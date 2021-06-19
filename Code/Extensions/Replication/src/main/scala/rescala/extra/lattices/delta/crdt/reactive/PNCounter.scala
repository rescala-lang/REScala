package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.PNCounterInterface
import rescala.extra.lattices.delta.interfaces.PNCounterInterface.{PNCounterCompanion, State}

/** [[ReactiveCRDT Reactive]] implementation of [[PNCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class PNCounter(
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends PNCounterInterface[PNCounter] with ReactiveCRDT[State, PNCounter] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): PNCounter =
    new PNCounter(state, replicaID, deltaBuffer)
}

object PNCounter extends PNCounterCompanion {

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply(replicaID: String): PNCounter = new PNCounter(UIJDLattice[State].bottom, replicaID, List())
}
