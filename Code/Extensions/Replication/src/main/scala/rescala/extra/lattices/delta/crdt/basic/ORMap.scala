package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.interfaces.ORMapInterface.{ORMapCompanion, State}
import rescala.extra.lattices.delta.interfaces.ORMapInterface
import rescala.extra.lattices.delta.{CContext, DotStore, UIJDLattice}

class ORMap[K, V: DotStore, C: CContext](
    val state: State[K, V, C],
    protected val antiEntropy: AntiEntropy[State[K, V, C]]
) extends ORMapInterface[K, V, C, ORMap[K, V, C]] with BasicCRDT[State[K, V, C], ORMap[K, V, C]] {

  override protected def copy(state: State[K, V, C]): ORMap[K, V, C] = new ORMap(state, antiEntropy)
}

object ORMap extends ORMapCompanion {
  def apply[K, V: DotStore, C: CContext](antiEntropy: AntiEntropy[State[K, V, C]]): ORMap[K, V, C] =
    new ORMap(UIJDLattice[State[K, V, C]].bottom, antiEntropy)
}
