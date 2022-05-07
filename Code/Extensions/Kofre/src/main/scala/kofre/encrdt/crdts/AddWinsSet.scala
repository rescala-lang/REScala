package kofre.encrdt.crdts
import kofre.base.Lattice
import kofre.encrdt.lattices.AddWinsSetLattice

class AddWinsSet[T](val replicaId: String) {

  private var _state: AddWinsSetLattice[T] = AddWinsSetLattice[T]()

  def this(replicaId: String, initialState: AddWinsSetLattice[T]) = {
    this(replicaId)
    _state = initialState
  }

  private def state_=(state: AddWinsSetLattice[T]): Unit = {
    _state = state
  }

  def state: AddWinsSetLattice[T] = _state

  def merge(remoteState: AddWinsSetLattice[T]): Unit = Lattice.merge(state, remoteState)

  def add(element: T): Unit = {
    state = state.added(element, replicaId)
  }

  def remove(element: T): Unit = {
    state = state.removed(element)
  }

  def contains(element: T): Boolean = values contains element

  def values: Set[T] = state.values

}
