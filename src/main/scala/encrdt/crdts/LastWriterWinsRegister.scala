package de.ckuessner
package encrdt.crdts

import encrdt.lattices.{LWWTime, LastWriterWinsRegisterLattice, SemiLattice}

class LastWriterWinsRegister[T](initialState: LastWriterWinsRegisterLattice[T, LWWTime],
                                val replicaId: String) { // SemiLattice requires ordering of timestamp

  private var _state = initialState

  def state: LastWriterWinsRegisterLattice[T, LWWTime] = _state

  def value: T = state.value

  def set(value: T): Unit = {
    _state = LastWriterWinsRegisterLattice(value, _state.timestamp.advance(replicaId))
  }

  def merge(otherState: LastWriterWinsRegisterLattice[T, LWWTime]): Unit =
    _state = SemiLattice.merged(this.state, otherState)
}

object LastWriterWinsRegister {
  def apply[T](replicaId: String, initialValue: T): LastWriterWinsRegister[T] =
    new LastWriterWinsRegister(
      LastWriterWinsRegisterLattice(
        initialValue,
        LWWTime().advance(replicaId)),
      replicaId)
}