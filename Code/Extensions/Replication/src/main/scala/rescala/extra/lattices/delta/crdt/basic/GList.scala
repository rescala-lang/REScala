package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.GListInterface
import rescala.extra.lattices.delta.interfaces.GListInterface.{GListCompanion, State}

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.GListInterface GListInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam E Type of the elements in the list
  */
class GList[E](
    val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends GListInterface[E, GList[E]] with BasicCRDT[State[E], GList[E]] {

  override protected def copy(state: State[E]): GList[E] = new GList(state, antiEntropy)
}

object GList extends GListCompanion {

  /** Creates a new GList instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam E Type of the elements in the list
    */
  def apply[E](antiEntropy: AntiEntropy[State[E]]): GList[E] = new GList(UIJDLattice[State[E]].bottom, antiEntropy)
}
