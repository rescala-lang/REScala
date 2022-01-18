package kofre.encrdt.crdts
import kofre.Lattice
import kofre.encrdt.crdts.AddWinsLastWriterWinsMap.LatticeType
import kofre.encrdt.lattices.{AddWinsMapLattice, CausalTimeTag, LastWriterWinsRegisterLattice}

class AddWinsLastWriterWinsMap[K, V](
    val replicaId: String,
    initialState: AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]] =
      AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]]()
){

  private var _state = initialState

  def state: LatticeType[K, V] = _state

  def get(key: K): Option[V] = _state.values.get(key).map(reg => reg.value)

  def put(key: K, value: V): Unit = {
    val timeStamp = _state.values.get(key) match {
      case Some(register) => register.timestamp.advance(replicaId)
      case None           => CausalTimeTag().advance(replicaId)
    }

    _state = _state.added(key, LastWriterWinsRegisterLattice(value, timeStamp), replicaId)
  }

  def remove(key: K): Unit = _state = _state.removed(key)

  def values: Map[K, V] =
    _state.values.map { case (k, LastWriterWinsRegisterLattice(v, _)) => k -> v }

  def merge(otherState: LatticeType[K, V]): Unit = {
    _state = Lattice.merge(_state, otherState)
  }
}

object AddWinsLastWriterWinsMap {
  type LatticeType[K, V] = AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]]
}
