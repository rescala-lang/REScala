package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.DotSet
import rescala.extra.lattices.delta.interfaces.EWFlagInterface.{EWFlagCompanion, State}
import rescala.extra.lattices.delta.interfaces.EWFlagInterface
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

class EWFlag[C: CContext](
    protected[rescala] val state: State[C],
    protected val antiEntropy: AntiEntropy[State[C]]
) extends EWFlagInterface[C, EWFlag[C]] with BasicCRDT[State[C], EWFlag[C]] {

  override protected def copy(state: State[C]): EWFlag[C] = new EWFlag(state, antiEntropy)
}

object EWFlag extends EWFlagCompanion {
  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): EWFlag[C] =
    new EWFlag(UIJDLattice[State[C]].bottom, antiEntropy)
}
