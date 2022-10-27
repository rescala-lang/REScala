package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.crdts.interfaces.SetCrdt
import com.github.ckuessner.encrdt.lattices.{AddWinsSetLattice, SemiLattice}

class AddWinsSet[T](val replicaId: String) extends SetCrdt[T] {

  private var _state: AddWinsSetLattice[T] = AddWinsSetLattice[T]()

  def this(replicaId: String, initialState: AddWinsSetLattice[T]) = {
    this(replicaId)
    _state = initialState
  }

  private def state_=(state: AddWinsSetLattice[T]): Unit = {
    _state = state
  }

  def state: AddWinsSetLattice[T] = _state

  def merge(remoteState: AddWinsSetLattice[T]): Unit = SemiLattice.merged(state, remoteState)

  def add(element: T): Unit = {
    state = state.added(element, replicaId)
  }

  def remove(element: T): Unit = {
    state = state.removed(element)
  }

  def contains(element: T): Boolean = values contains element

  def values: Set[T] = state.values

}
