package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.{DotMap, DotSet}
import rescala.extra.lattices.delta.interfaces.AWSetInterface.{AWSetCompanion, State}
import rescala.extra.lattices.delta.interfaces.AWSetInterface
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

class AWSet[E, C: CContext](
    protected[rescala] val state: State[E, C],
    protected val antiEntropy: AntiEntropy[State[E, C]]
) extends AWSetInterface[E, C, AWSet[E, C]] with BasicCRDT[State[E, C], AWSet[E, C]] {

  override protected def copy(state: State[E, C]): AWSet[E, C] = new AWSet(state, antiEntropy)
}

object AWSet extends AWSetCompanion {
  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): AWSet[E, C] =
    new AWSet(UIJDLattice[State[E, C]].bottom, antiEntropy)
}
