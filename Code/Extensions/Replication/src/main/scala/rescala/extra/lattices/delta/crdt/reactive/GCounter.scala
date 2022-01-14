package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.interfaces.GCounterInterface
import kofre.decompose.interfaces.GCounterInterface.{GCounterCompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.GCounterInterface GCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class GCounter(
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends GCounterInterface[GCounter] with ReactiveCRDT[State, GCounter] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): GCounter =
    new GCounter(state, replicaID, deltaBuffer)
}

object GCounter extends GCounterCompanion {

  /** Creates a new GCounter instance
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply(replicaID: String): GCounter = new GCounter(UIJDLattice[State].bottom, replicaID, List())
}
