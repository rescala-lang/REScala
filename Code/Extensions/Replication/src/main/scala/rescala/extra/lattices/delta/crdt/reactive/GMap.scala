package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.extra.lattices.delta.interfaces.GMapInterface
import rescala.extra.lattices.delta.interfaces.GMapInterface.{GMapCompanion, State}

/** [[ReactiveCRDT Reacitve]] implementation of [[GMapInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam K Type of the keys of this map
  * @tparam V State type of the nested CRDTs
  */
class GMap[K, V: UIJDLattice](
    val state: State[K, V],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[K, V]]]
) extends GMapInterface[K, V, GMap[K, V]] with ReactiveCRDT[State[K, V], GMap[K, V]] {

  override protected def copy(state: State[K, V], deltaBuffer: List[Delta[State[K, V]]]): GMap[K, V] =
    new GMap(state, replicaID, deltaBuffer)
}

object GMap extends GMapCompanion {

  /** Creates a new GMap instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam K Type of the keys of this map
    * @tparam V State type of the nested CRDTs
    */
  def apply[K, V: UIJDLattice](replicaID: String): GMap[K, V] =
    new GMap(UIJDLattice[State[K, V]].bottom, replicaID, List())
}
