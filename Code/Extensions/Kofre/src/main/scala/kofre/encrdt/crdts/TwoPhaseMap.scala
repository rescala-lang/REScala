
package encrdt.crdts
import kofre.Lattice
import encrdt.crdts.interfaces.MapCrdt
import encrdt.lattices.{TwoPhaseMapLattice}

class TwoPhaseMap[K, V: Lattice](val replicaId: String,
                                 val initialState: TwoPhaseMapLattice[K, V]) extends MapCrdt[K, V] {

  private var _state = initialState

  def state: TwoPhaseMapLattice[K, V] = _state

  override def get(key: K): Option[V] = _state.get(key)

  override def put(key: K, value: V): Unit = _state = _state.added(key, value)

  override def remove(key: K): Unit = _state = _state.removed(key)

  override def values: Map[K, V] = _state.values
}
