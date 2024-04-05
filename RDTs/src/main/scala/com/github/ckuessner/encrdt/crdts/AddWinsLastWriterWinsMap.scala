package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.crdts.AddWinsLastWriterWinsMap.LatticeType
import com.github.ckuessner.encrdt.crdts.interfaces.MapCrdt
import com.github.ckuessner.encrdt.lattices.{
  AddWinsMapLattice,
  CausalTimeTag,
  LastWriterWinsRegisterLattice,
  SemiLattice
}

class AddWinsLastWriterWinsMap[K, V](
    val replicaId: String,
    initialState: AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]] =
      AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]]()
) extends MapCrdt[K, V] {

  private var _state = initialState

  def state: LatticeType[K, V] = _state

  override def get(key: K): Option[V] = _state.values.get(key).map(reg => reg.value)

  override def put(key: K, value: V): Unit = {
    val timeStamp = _state.values.get(key) match {
      case Some(register) => register.timestamp.advance(replicaId)
      case None           => CausalTimeTag().advance(replicaId)
    }

    _state = _state.added(key, LastWriterWinsRegisterLattice(value, timeStamp), replicaId)
  }

  override def remove(key: K): Unit = _state = _state.removed(key)

  override def values: Map[K, V] =
    _state.values.map { case (k, LastWriterWinsRegisterLattice(v, _)) => k -> v }

  def merge(otherState: LatticeType[K, V]): Unit = {
    _state = SemiLattice.merged(_state, otherState)
  }
}

object AddWinsLastWriterWinsMap {
  type LatticeType[K, V] = AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]]
}
