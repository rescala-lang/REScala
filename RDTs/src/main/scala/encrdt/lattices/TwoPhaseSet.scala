package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.{SemiLattice, SetCrdt}

class TwoPhaseSet[T](val replicaId: String) extends SetCrdt[T] {

  private var _state = TwoPhaseSetLattice[T]()

  def state: TwoPhaseSetLattice[T] = _state

  def merge(remoteState: TwoPhaseSetLattice[T]): Unit = {
    _state = SemiLattice[TwoPhaseSetLattice[T]].merged(state, remoteState)
  }

  def add(element: T): Unit = {
    _state = _state.added(element)
  }

  /**
   * Permanently removes the element from the Set.
   * When removing an element that is not currently present in the Set, the element can't be added later on.
   *
   * @param element The element to be removed
   */
  def remove(element: T): Unit = {
    _state = _state.removed(element)
  }

  def values: Set[T] = state.values
}

case class TwoPhaseSetLattice[T](addedElems: Set[T] = Set[T](), removedElems: Set[T] = Set[T]()) {
  def values: Set[T] = addedElems -- removedElems

  def added(element: T): TwoPhaseSetLattice[T] = copy(addedElems = addedElems + element)

  def removed(element: T): TwoPhaseSetLattice[T] = {
    copy(removedElems = removedElems + element)
  }
}

object TwoPhaseSetLattice {
  implicit def TwoPhaseSetSemiLattice[T]: SemiLattice[TwoPhaseSetLattice[T]] =
    (left: TwoPhaseSetLattice[T], right: TwoPhaseSetLattice[T]) =>
      TwoPhaseSetLattice(left.addedElems ++ right.addedElems, left.removedElems ++ right.removedElems)
}
