package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.causality.VectorClock
import com.github.ckuessner.encrdt.lattices.{MultiValueRegisterLattice, SemiLattice}

class MultiValueRegister[T](initialState: MultiValueRegisterLattice[T], val replicaId: String) {
  private var _state = initialState

  def currentTime: VectorClock = {
    if (state.versions.isEmpty) VectorClock()
    else state.versions.keys.reduce((a, b) => a.merged(b))
  }

  def state: MultiValueRegisterLattice[T] = _state

  def values: List[T] = state.versions.values.toList

  def set(value: T): Unit = {
    val timeOfUpdate = currentTime.advance(replicaId)
    _state = MultiValueRegisterLattice(Map(timeOfUpdate -> value))
  }

  def merge(otherState: MultiValueRegisterLattice[T]): Unit =
    _state = SemiLattice.merged(this.state, otherState)
}
