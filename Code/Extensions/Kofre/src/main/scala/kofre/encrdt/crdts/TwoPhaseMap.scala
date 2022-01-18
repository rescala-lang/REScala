package kofre.encrdt.crdts
import kofre.Lattice
import kofre.encrdt.lattices.TwoPhaseMapLattice

class TwoPhaseMap[K, V: Lattice](val replicaId: String, val initialState: TwoPhaseMapLattice[K, V]) {

  private var _state = initialState

  def state: TwoPhaseMapLattice[K, V] = _state

  def get(key: K): Option[V] = _state.get(key)

  def put(key: K, value: V): Unit = _state = _state.added(key, value)

  def remove(key: K): Unit = _state = _state.removed(key)

  def values: Map[K, V] = _state.values
}
