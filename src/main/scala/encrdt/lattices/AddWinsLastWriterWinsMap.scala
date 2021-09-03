package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.{MapCrdt, SemiLattice}

class AddWinsLastWriterWinsMap[K, V](val replicaId: String,
                                     initialState: AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, LWWTime]] = AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, LWWTime]]()
                                    ) extends MapCrdt[K, V] {

  private var _state = initialState

  def state: AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, LWWTime]] = _state

  override def get(key: K): Option[V] = _state.values.get(key).map(reg => reg.value)

  override def put(key: K, value: V): Unit = {
    val timeStamp = _state.values.get(key) match {
      case Some(register) => register.timestamp.advance(replicaId)
      case None => LWWTime().advance(replicaId)
    }

    _state = _state.added(key, LastWriterWinsRegisterLattice(value, timeStamp), replicaId)
  }

  override def remove(key: K): Unit = _state = _state.removed(key)

  override def values: Map[K, V] = _state.values.map { case (k, LastWriterWinsRegisterLattice(v, _)) => k -> v }
}

case class AddWinsMapLattice[K, V](keys: AddWinsSetLattice[K] = AddWinsSetLattice[K](), mappings: Map[K, V] = Map[K, V]()) {
  def values: Map[K, V] = mappings

  def added(key: K, value: V, replicaId: String): AddWinsMapLattice[K, V] = {
    val newKeys = keys.added(key, replicaId)
    val newMap = mappings + (key -> value)
    AddWinsMapLattice(newKeys, newMap)
  }

  def removed(key: K): AddWinsMapLattice[K, V] = {
    val newKeys = keys.removed(key)
    val newMap = mappings - key
    AddWinsMapLattice(newKeys, newMap)
  }
}

object AddWinsMapLattice {
  implicit def AddWinsLattice[K, V: SemiLattice]: SemiLattice[AddWinsMapLattice[K, V]] =
    (left: AddWinsMapLattice[K, V], right: AddWinsMapLattice[K, V]) => {
      val keys = SemiLattice.merged(left.keys, right.keys)
      val mappings = keys.values()
        .map(k => (left.mappings.get(k), right.mappings.get(k)) match {
          case (Some(l), None) => k -> l
          case (None, Some(r)) => k -> r
          case (Some(l), Some(r)) => k -> SemiLattice.merged(l, r)
        }).toMap
      AddWinsMapLattice(keys, mappings)
    }
}