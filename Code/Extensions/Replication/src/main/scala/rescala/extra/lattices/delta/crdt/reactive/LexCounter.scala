package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.LexCounterInterface.{LexCounterCompanion, State}
import rescala.extra.lattices.delta.interfaces.LexCounterInterface

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.LexCounterInterface LexCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class LexCounter(
    val state: State,
    val replicaID: String,
    val deltaBuffer: List[Delta[State]]
) extends LexCounterInterface[LexCounter] with ReactiveCRDT[State, LexCounter] {

  override protected def copy(state: State, deltaBuffer: List[Delta[State]]): LexCounter =
    new LexCounter(state, replicaID, deltaBuffer)
}

object LexCounter extends LexCounterCompanion {

  /** Creates a new LexCounter instance
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply(replicaID: String): LexCounter = new LexCounter(UIJDLattice[State].bottom, replicaID, List())
}
