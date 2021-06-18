package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.TwoPSetInterface
import rescala.extra.lattices.delta.interfaces.TwoPSetInterface.{State, TwoPSetCompanion}

/** [[BasicCRDT Basic]] implementation of [[TwoPSetInterface]]
  *
  * @tparam E Type of the elements stored in the set
  */
class TwoPSet[E](
    val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends TwoPSetInterface[E, TwoPSet[E]] with BasicCRDT[State[E], TwoPSet[E]] {

  override protected def copy(state: (Set[E], Set[E])): TwoPSet[E] = new TwoPSet(state, antiEntropy)
}

object TwoPSet extends TwoPSetCompanion {

  /** Creates a new TwoPSet instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam E Type of the elements stored in the set
    */
  def apply[E](antiEntropy: AntiEntropy[State[E]]): TwoPSet[E] = new TwoPSet(UIJDLattice[State[E]].bottom, antiEntropy)
}
