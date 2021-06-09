package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.LexCounterInterface.{LexCounterCompanion, State}
import rescala.extra.lattices.delta.interfaces.LexCounterInterface

class LexCounter(
    val state: State,
    protected val antiEntropy: AntiEntropy[State]
) extends LexCounterInterface[LexCounter] with BasicCRDT[State, LexCounter] {

  override protected def copy(state: State = state): LexCounter = new LexCounter(state, antiEntropy)
}

object LexCounter extends LexCounterCompanion {
  def apply(antiEntropy: AntiEntropy[State]): LexCounter = new LexCounter(UIJDLattice[State].bottom, antiEntropy)
}
