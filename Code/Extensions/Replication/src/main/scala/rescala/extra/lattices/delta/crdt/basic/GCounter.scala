package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.GCounterInterface.{GCounterCompanion, State}
import rescala.extra.lattices.delta.interfaces.GCounterInterface

class GCounter(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends GCounterInterface[GCounter] with BasicCRDT[State, GCounter] {

  override protected def copy(state: State): GCounter = new GCounter(state, antiEntropy)
}

object GCounter extends GCounterCompanion {
  def apply(antiEntropy: AntiEntropy[State]): GCounter = new GCounter(UIJDLattice[State].bottom, antiEntropy)
}
