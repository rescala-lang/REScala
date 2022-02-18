package kofre.encrdt.crdts
import kofre.Lattice
import kofre.encrdt.lattices.{CausalTimeTag, LastWriterWins}

class LastWriterWinsRegister[T](initialState: LastWriterWins[T, CausalTimeTag], val replicaId: String) {

  private var _state = initialState

  def state: LastWriterWins[T, CausalTimeTag] = _state

  def value: T = state.payload

  def set(value: T): Unit = {
    _state = LastWriterWins(value, _state.timestamp.advance(replicaId))
  }

  def merge(otherState: LastWriterWins[T, CausalTimeTag]): Unit =
    _state = Lattice.merge(this.state, otherState)
}

object LastWriterWinsRegister {
  def apply[T](replicaId: String, initialValue: T): LastWriterWinsRegister[T] =
    new LastWriterWinsRegister(
      LastWriterWins(initialValue, CausalTimeTag().advance(replicaId)),
      replicaId
    )
}
