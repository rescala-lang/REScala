package encrdtlib.container

import encrdtlib.container.AddWinsLastWriterWinsMap.LatticeType
import encrdtlib.lattices.AddWinsMapLattice
import kofre.base.{Lattice, Uid}
import kofre.datatypes.LastWriterWins
import kofre.time.CausalTime

class AddWinsLastWriterWinsMap[K, V](
    val replicaId: Uid,
    initialState: AddWinsMapLattice[K, LastWriterWins[V]] =
      AddWinsMapLattice[K, LastWriterWins[V]]()
) {

  private var _state: AddWinsMapLattice[K, LastWriterWins[V]] = initialState

  def state: LatticeType[K, V] = _state

  def get(key: K): Option[V] = _state.values.get(key).map(reg => reg.payload)

  def put(key: K, value: V): Unit = {
    val timeStamp = _state.values.get(key) match {
      case Some(register) => register.timestamp.advance
      case None           => CausalTime.now()
    }

    _state = _state.added(key, LastWriterWins(timeStamp, value), replicaId)
  }

  def remove(key: K): Unit = _state = _state.removed(key)

  def values: Map[K, V] =
    _state.values.map { case (k, LastWriterWins(_, v)) => k -> v }

  def merge(otherState: LatticeType[K, V]): Unit = {
    _state = Lattice.merge(_state, otherState)
  }
}

object AddWinsLastWriterWinsMap {
  type LatticeType[K, V] = AddWinsMapLattice[K,  LastWriterWins[V]]
}
