package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.lattices.{CausalTimeTag, LastWriterWinsRegisterLattice, SemiLattice}

class LastWriterWinsRegister[T](initialState: LastWriterWinsRegisterLattice[T, CausalTimeTag],
                                val replicaId: String) { // SemiLattice requires ordering of timestamp

  private var _state = initialState

  def state: LastWriterWinsRegisterLattice[T, CausalTimeTag] = _state

  def value: T = state.value

  def set(value: T): Unit = {
    _state = LastWriterWinsRegisterLattice(value, _state.timestamp.advance(replicaId))
  }

  def merge(otherState: LastWriterWinsRegisterLattice[T, CausalTimeTag]): Unit =
    _state = SemiLattice.merged(this.state, otherState)
}

object LastWriterWinsRegister {
  def apply[T](replicaId: String, initialValue: T): LastWriterWinsRegister[T] =
    new LastWriterWinsRegister(
      LastWriterWinsRegisterLattice(
        initialValue,
        CausalTimeTag().advance(replicaId)),
      replicaId)
}
