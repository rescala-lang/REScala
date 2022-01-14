package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.UIJDLattice
import kofre.decompose.interfaces.GCounterInterface
import kofre.decompose.interfaces.GCounterInterface.{GCounterCompanion, State}
import rescala.extra.replication.AntiEntropy

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.GCounterInterface GCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class GCounter(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends GCounterInterface[GCounter] with BasicCRDT[State, GCounter] {

  override protected def copy(state: State): GCounter = new GCounter(state, antiEntropy)
}

object GCounter extends GCounterCompanion {

  /** Creates a new GCounter instance
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply(antiEntropy: AntiEntropy[State]): GCounter = new GCounter(UIJDLattice[State].bottom, antiEntropy)
}
