package rescala.extra.lattices.delta.crdt.reactive

import rescala.extra.lattices.delta.DotStore.DotMap
import rescala.extra.lattices.delta.interfaces.ORMapInterface.{ORMapCompanion, State}
import rescala.extra.lattices.delta.interfaces.ORMapInterface
import rescala.extra.lattices.delta.{CContext, Delta, DotStore, UIJDLattice}

class ORMap[K, V: DotStore, C: CContext](
    protected[rescala] val state: State[K, V, C],
    protected val replicaID: String,
    protected[rescala] val deltaBuffer: List[Delta[State[K, V, C]]]
) extends ORMapInterface[K, V, C, ORMap[K, V, C]] with ReactiveCRDT[State[K, V, C], ORMap[K, V, C]] {

  override protected def copy(state: State[K, V, C], deltaBuffer: List[Delta[State[K, V, C]]]): ORMap[K, V, C] =
    new ORMap(state, replicaID, deltaBuffer)
}

object ORMap extends ORMapCompanion {
  def apply[K, V: DotStore, C: CContext](replicaID: String): ORMap[K, V, C] =
    new ORMap(UIJDLattice[State[K, V, C]].bottom, replicaID, List())
}
