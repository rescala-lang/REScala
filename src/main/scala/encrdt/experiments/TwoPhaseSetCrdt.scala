package de.ckuessner
package encrdt.experiments

class TwoPhaseSetCrdt[T](val replicaId: Int) extends Crdt {
  override type StateT = TwoPhaseSetState[T]

  private var _state = TwoPhaseSetState[T]()

  override def state: TwoPhaseSetState[T] = _state

  override def merge(remote: TwoPhaseSetState[T]): Unit = {
    _state = state.merged(remote)
  }

  def add(element: T): Unit = {
    _state = _state.added(element)
  }

  // When removing an element that is not currently present in the Set, the element can't be added later on.
  def remove(element: T): Unit = {
    _state = _state.removed(element)
  }
}

case class TwoPhaseSetState[T](added: Set[T] = Set(), removed: Set[T] = Set()) {
  def merged(remote: TwoPhaseSetState[T]): TwoPhaseSetState[T] =
    TwoPhaseSetState(added ++ remote.added, removed ++ remote.removed)

  def elements: Set[T] = added -- removed

  def added(element: T): TwoPhaseSetState[T] = copy(added = added + element)

  def removed(element: T): TwoPhaseSetState[T] = {
    copy(removed = removed + element)
  }
}
