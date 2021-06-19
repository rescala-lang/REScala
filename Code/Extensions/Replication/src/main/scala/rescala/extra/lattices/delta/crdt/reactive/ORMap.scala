package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotMap
import rescala.extra.lattices.delta.interfaces.ORMapInterface.{ORMapCompanion, State}
import rescala.extra.lattices.delta.interfaces.ORMapInterface
import rescala.extra.lattices.delta.{CContext, Delta, DotStore, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.ORMapInterface ORMapInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam K Type of the keys of this map
  * @tparam V Type of the dot store used as values in this map
  * @tparam C Type of the causal context used for this causal CRDT
  */
class ORMap[K, V: DotStore, C: CContext](
    val state: State[K, V, C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[K, V, C]]]
) extends ORMapInterface[K, V, C, ORMap[K, V, C]] with ReactiveCRDT[State[K, V, C], ORMap[K, V, C]] {

  override protected def copy(state: State[K, V, C], deltaBuffer: List[Delta[State[K, V, C]]]): ORMap[K, V, C] =
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
  def apply[K, V: DotStore, C: CContext](replicaID: String): ORMap[K, V, C] =
    new ORMap(UIJDLattice[State[K, V, C]].bottom, replicaID, List())
}
