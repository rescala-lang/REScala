package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.PNCounterInterface
import rescala.extra.lattices.delta.interfaces.PNCounterInterface.{PNCounterCompanion, State}

class PNCounter(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends PNCounterInterface[PNCounter] with BasicCRDT[State, PNCounter] {

  override protected def copy(state: State): PNCounter = new PNCounter(state, antiEntropy)
}

object PNCounter extends PNCounterCompanion {
  def apply(antiEntropy: AntiEntropy[State]): PNCounter = new PNCounter(UIJDLattice[State].bottom, antiEntropy)
}
