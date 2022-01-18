package kofre.encrdt.crdts
import kofre.Lattice
import kofre.Lattice.Operators
import kofre.encrdt.lattices.MultiValueRegisterLattice
import kofre.primitives.VectorClock

class MultiValueRegister[T](initialState: MultiValueRegisterLattice[T], val replicaId: String) {
  private var _state = initialState

  def currentTime: VectorClock = {
    if (state.versions.isEmpty) VectorClock.zero
    else state.versions.keys.reduce((a, b) => a.merge(b))
  }

  def state: MultiValueRegisterLattice[T] = _state

  def values: List[T] = state.versions.values.toList

  def set(value: T): Unit = {
    val timeOfUpdate = currentTime.advance(replicaId)
    _state = MultiValueRegisterLattice(Map(timeOfUpdate -> value))
  }

  def merge(otherState: MultiValueRegisterLattice[T]): Unit =
    _state = Lattice.merge(this.state, otherState)
}
