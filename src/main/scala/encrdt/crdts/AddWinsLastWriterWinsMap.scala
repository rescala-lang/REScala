package de.ckuessner
package encrdt.crdts

import encrdt.crdts.interfaces.MapCrdt
import encrdt.lattices.{AddWinsMapLattice, LWWTime, LastWriterWinsRegisterLattice}

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



