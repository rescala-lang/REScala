package kofre.encrdt.crdts


class GrowOnlySet[T] {
  private var _state: Set[T] = Set()

  def state: Set[T] = _state

  def add(element: T): Unit = _state += element

  def remove(element: T): Unit =
    throw new UnsupportedOperationException("Can't remove elements from GrowOnlySet")

  def values: Set[T] = _state
}
