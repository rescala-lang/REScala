package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.GSetInterface.{GSetCompanion, State}
import rescala.extra.lattices.delta.interfaces.GSetInterface

class GSet[E](
    protected[rescala] val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends GSetInterface[E, GSet[E]] with BasicCRDT[State[E], GSet[E]] {

  override protected def copy(state: State[E]): GSet[E] = new GSet(state, antiEntropy)
}

object GSet extends GSetCompanion {
  def apply[E](antiEntropy: AntiEntropy[State[E]]): GSet[E] = new GSet(UIJDLattice[State[E]].bottom, antiEntropy)
}
