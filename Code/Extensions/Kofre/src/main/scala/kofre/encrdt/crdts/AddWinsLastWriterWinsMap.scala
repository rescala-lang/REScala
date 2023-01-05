package kofre.encrdt.crdts
import kofre.base.{Defs, Lattice}
import kofre.encrdt.crdts.AddWinsLastWriterWinsMap.LatticeType
import kofre.encrdt.lattices.{AddWinsMapLattice, CausalTimeTag}
import kofre.primitives.LastWriterWins

class AddWinsLastWriterWinsMap[K, V](
    val replicaId: Defs.Id,
    initialState: AddWinsMapLattice[K, LastWriterWins[CausalTimeTag, V]] =
      AddWinsMapLattice[K, LastWriterWins[CausalTimeTag, V]]()
) {

  private var _state: AddWinsMapLattice[K, LastWriterWins[CausalTimeTag, V]] = initialState

  def state: LatticeType[K, V] = _state

  def get(key: K): Option[V] = _state.values.get(key).map(reg => reg.payload)

  def put(key: K, value: V): Unit = {
    val timeStamp = _state.values.get(key) match {
      case Some(register) => register.timestamp.advance(replicaId)
      case None           => CausalTimeTag(replicaId = replicaId).advance(replicaId)
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
  type LatticeType[K, V] = AddWinsMapLattice[K, LastWriterWins[CausalTimeTag, V]]
}
