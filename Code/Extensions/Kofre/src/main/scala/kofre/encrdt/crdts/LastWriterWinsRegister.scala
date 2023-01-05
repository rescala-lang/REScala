package kofre.encrdt.crdts
import kofre.base.{Id, Lattice}
import kofre.encrdt.lattices.CausalTimeTag
import kofre.primitives.LastWriterWins

class LastWriterWinsRegister[T](initialState: LastWriterWins[CausalTimeTag, T], val replicaId: Id) {

  private var _state = initialState

  def state: LastWriterWins[CausalTimeTag, T] = _state

  def value: T = state.payload

  def set(value: T): Unit = {
    _state = LastWriterWins(_state.timestamp.advance(replicaId), value)
  }

  def merge(otherState: LastWriterWins[CausalTimeTag, T]): Unit =
    _state = Lattice.merge(this.state, otherState)
}

object LastWriterWinsRegister {
  def apply[T](replicaId: Id, initialValue: T): LastWriterWinsRegister[T] =
    new LastWriterWinsRegister(
      LastWriterWins(CausalTimeTag(replicaId = replicaId).advance(replicaId), initialValue),
      replicaId
    )
}
