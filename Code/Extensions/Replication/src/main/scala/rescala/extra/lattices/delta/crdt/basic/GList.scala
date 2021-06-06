package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.GListInterface
import rescala.extra.lattices.delta.interfaces.GListInterface.{GListCompanion, State}

class GList[E](
    protected[rescala] val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends GListInterface[E, GList[E]] with BasicCRDT[State[E], GList[E]] {

  override protected def copy(state: State[E]): GList[E] = new GList(state, antiEntropy)
}

object GList extends GListCompanion {
  def apply[E](antiEntropy: AntiEntropy[State[E]]): GList[E] = new GList(UIJDLattice[State[E]].bottom, antiEntropy)
}
