package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.TwoPSetInterface
import rescala.extra.lattices.delta.interfaces.TwoPSetInterface.{State, TwoPSetCompanion}

class TwoPSet[E](
    val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends TwoPSetInterface[E, TwoPSet[E]] with BasicCRDT[State[E], TwoPSet[E]] {

  override protected def copy(state: (Set[E], Set[E])): TwoPSet[E] = new TwoPSet(state, antiEntropy)
}

object TwoPSet extends TwoPSetCompanion {
  def apply[E](antiEntropy: AntiEntropy[State[E]]): TwoPSet[E] = new TwoPSet(UIJDLattice[State[E]].bottom, antiEntropy)
}
