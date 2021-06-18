package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.interfaces.ORMapInterface.{ORMapCompanion, State}
import rescala.extra.lattices.delta.interfaces.ORMapInterface
import rescala.extra.lattices.delta.{CContext, DotStore, UIJDLattice}

/** [[BasicCRDT Basic]] implementation of [[ORMapInterface]]
  *
  * @tparam K Type of the keys of this map
  * @tparam V Type of the dot store used as values in this map
  * @tparam C Type of the causal context used for this causal CRDT
  */
class ORMap[K, V: DotStore, C: CContext](
    val state: State[K, V, C],
    protected val antiEntropy: AntiEntropy[State[K, V, C]]
) extends ORMapInterface[K, V, C, ORMap[K, V, C]] with BasicCRDT[State[K, V, C], ORMap[K, V, C]] {

  override protected def copy(state: State[K, V, C]): ORMap[K, V, C] = new ORMap(state, antiEntropy)
}

object ORMap extends ORMapCompanion {

  /** Creates a new ORMap instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    * @tparam K Type of the keys of this map
    * @tparam V Type of the dot store used as values in this map. Usually, this will be the Embedded type defined by a causal CRDT.
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[K, V: DotStore, C: CContext](antiEntropy: AntiEntropy[State[K, V, C]]): ORMap[K, V, C] =
    new ORMap(UIJDLattice[State[K, V, C]].bottom, antiEntropy)
}
