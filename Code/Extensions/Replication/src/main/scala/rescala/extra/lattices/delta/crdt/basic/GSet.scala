package rescala.extra.lattices.delta.crdt.basic

import kofre.decompose.UIJDLattice
import kofre.decompose.interfaces.GSetInterface
import kofre.decompose.interfaces.GSetInterface.{GSetCompanion, State}

/** [[BasicCRDT Basic]] implementation of [[rescala.extra.lattices.delta.interfaces.GSetInterface GSetInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam E Type of the elements stored in the set
  */
class GSet[E](
    val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends GSetInterface[E, GSet[E]] with BasicCRDT[State[E], GSet[E]] {

  override protected def copy(state: State[E]): GSet[E] = new GSet(state, antiEntropy)
}

object GSet extends GSetCompanion {

  /** Creates a new GSet instance
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam E Type of the elements stored in the set
    */
  def apply[E](antiEntropy: AntiEntropy[State[E]]): GSet[E] = new GSet(UIJDLattice[State[E]].bottom, antiEntropy)
}
