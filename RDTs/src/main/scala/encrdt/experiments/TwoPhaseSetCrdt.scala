package de.ckuessner
package encrdt.experiments

class TwoPhaseSetCrdt[T](val replicaId: Int) {

  private var _state = TwoPhaseSetState[T]()

  def state: TwoPhaseSetState[T] = _state

  def merge(remoteState: TwoPhaseSetState[T]): Unit = {
    _state = SemiLattice[TwoPhaseSetState[T]].merged(state, remoteState)
  }

  def add(element: T): Unit = {
    _state = _state.added(element)
  }

  // When removing an element that is not currently present in the Set, the element can't be added later on.
  def remove(element: T): Unit = {
    _state = _state.removed(element)
  }
}

case class TwoPhaseSetState[T](added: Set[T] = Set[T](), removed: Set[T] = Set[T]()) {
  def elements: Set[T] = added -- removed

  def added(element: T): TwoPhaseSetState[T] = copy(added = added + element)

  def removed(element: T): TwoPhaseSetState[T] = {
    copy(removed = removed + element)
  }
}

object TwoPhaseSetState {
  implicit def TwoPhaseSetSemiLattice[T]: SemiLattice[TwoPhaseSetState[T]] =
    (left: TwoPhaseSetState[T], right: TwoPhaseSetState[T]) =>
      TwoPhaseSetState(left.added ++ right.added, left.removed ++ right.removed)
}
