package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.SetCrdt

class TwoPhaseSet[T](val replicaId: Int) extends SetCrdt[T] {

  private var _state = TwoPhaseSetLattice[T]()

  def state: TwoPhaseSetLattice[T] = _state

  def merge(remoteState: TwoPhaseSetLattice[T]): Unit = {
    _state = SemiLattice[TwoPhaseSetLattice[T]].merged(state, remoteState)
  }

  def add(element: T): Unit = {
    _state = _state.added(element)
  }

  // When removing an element that is not currently present in the Set, the element can't be added later on.
  def remove(element: T): Unit = {
    _state = _state.removed(element)
  }

  def values: Set[T] = state.values
}

case class TwoPhaseSetLattice[T](added: Set[T] = Set[T](), removed: Set[T] = Set[T]()) {
  def values: Set[T] = added -- removed

  def added(element: T): TwoPhaseSetLattice[T] = copy(added = added + element)

  def removed(element: T): TwoPhaseSetLattice[T] = {
    copy(removed = removed + element)
  }
}

object TwoPhaseSetLattice {
  implicit def TwoPhaseSetSemiLattice[T]: SemiLattice[TwoPhaseSetLattice[T]] =
    (left: TwoPhaseSetLattice[T], right: TwoPhaseSetLattice[T]) =>
      TwoPhaseSetLattice(left.added ++ right.added, left.removed ++ right.removed)
}
