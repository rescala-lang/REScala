package encrdtlib.container

import encrdtlib.container.AddWinsLastWriterWinsMap.LatticeType
import encrdtlib.lattices.{AddWinsMapLattice, CausalTimeTag}
import kofre.base.{Lattice, Uid}
import kofre.datatypes.alternatives.lww.GenericLastWriterWins

class AddWinsLastWriterWinsMap[K, V](
    val replicaId: Uid,
    initialState: AddWinsMapLattice[K, GenericLastWriterWins[CausalTimeTag, V]] =
      AddWinsMapLattice[K, GenericLastWriterWins[CausalTimeTag, V]]()
) {

  private var _state: AddWinsMapLattice[K, GenericLastWriterWins[CausalTimeTag, V]] = initialState

  def state: LatticeType[K, V] = _state

  def get(key: K): Option[V] = _state.values.get(key).map(reg => reg.payload)

  def put(key: K, value: V): Unit = {
    val timeStamp = _state.values.get(key) match {
      case Some(register) => register.timestamp.advance(replicaId)
      case None           => CausalTimeTag(replicaId = replicaId).advance(replicaId)
    }

    _state = _state.added(key, GenericLastWriterWins(timeStamp, value), replicaId)
  }

  def remove(key: K): Unit = _state = _state.removed(key)

  def values: Map[K, V] =
    _state.values.map { case (k, GenericLastWriterWins(_, v)) => k -> v }

  def merge(otherState: LatticeType[K, V]): Unit = {
    _state = Lattice.merge(_state, otherState)
  }
}

object AddWinsLastWriterWinsMap {
  type LatticeType[K, V] = AddWinsMapLattice[K, GenericLastWriterWins[CausalTimeTag, V]]
}
