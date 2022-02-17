package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.DotStore.DotMap
import kofre.decompose.interfaces.ORMapInterface
import kofre.decompose.interfaces.ORMapInterface.{ORMapCompanion, State}
import kofre.decompose.{Delta, DotStore, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.ORMapInterface ORMapInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam K Type of the keys of this map
  * @tparam V Type of the dot store used as values in this map
  * @tparam C Type of the causal context used for this causal CRDT
  */
class ORMap[K, V: DotStore](
    val state: State[K, V],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[K, V]]]
) extends ORMapInterface[K, V, ORMap[K, V]] with ReactiveCRDT[State[K, V], ORMap[K, V]] {

  override protected def copy(state: State[K, V], deltaBuffer: List[Delta[State[K, V]]]): ORMap[K, V] =
    new ORMap(state, replicaID, deltaBuffer)
}

object ORMap extends ORMapCompanion {

  /** Creates a new ORMap instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam K Type of the keys of this map
    * @tparam V Type of the dot store used as values in this map. Usually, this will be the Embedded type defined by a causal CRDT.
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[K, V: DotStore](replicaID: String): ORMap[K, V] =
    new ORMap(UIJDLattice[State[K, V]].bottom, replicaID, List())
}
