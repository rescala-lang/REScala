package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.interfaces.GMapInterface
import rescala.extra.lattices.delta.interfaces.GMapInterface.{GMapCompanion, State}

/** [[BasicCRDT Basic]] implementation of [[GMapInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam K Type of the keys of this map
  * @tparam V State type of the nested CRDTs
  */
class GMap[K, V: UIJDLattice](
    val state: State[K, V],
    protected val antiEntropy: AntiEntropy[State[K, V]]
) extends GMapInterface[K, V, GMap[K, V]] with BasicCRDT[State[K, V], GMap[K, V]] {

  override protected def copy(state: State[K, V]): GMap[K, V] = new GMap(state, antiEntropy)
}

object GMap extends GMapCompanion {

  /** Creates a new GMap instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam K Type of the keys of this map
    * @tparam V State type of the nested CRDTs
    */
  def apply[K, V: UIJDLattice](antiEntropy: AntiEntropy[State[K, V]]): GMap[K, V] =
    new GMap(UIJDLattice[State[K, V]].bottom, antiEntropy)
}
