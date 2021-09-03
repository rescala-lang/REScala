package de.ckuessner
package encrdt.lattices

import encrdt.causality.{LamportClock, VectorClock}
import encrdt.lattices.interfaces.{SemiLattice, SetCrdt}

class AddWinsSet[T](val replicaId: String) extends SetCrdt[T] {

  private var _state: AddWinsSetLattice[T] = AddWinsSetLattice[T]()

  def this(replicaId: String, initialState: AddWinsSetLattice[T]) = {
    this(replicaId)
    _state = initialState
  }

  private def state_=(state: AddWinsSetLattice[T]): Unit = {
    _state = state
  }

  def state: AddWinsSetLattice[T] = _state

  def merge(remoteState: AddWinsSetLattice[T]): Unit = SemiLattice.merged(state, remoteState)

  def add(element: T): Unit = {
    state = state.added(element, replicaId)
  }

  def remove(element: T): Unit = {
    state = state.removed(element)
  }

  def contains(element: T): Boolean = values contains element

  def values: Set[T] = state.values()

}

case class AddWinsSetLattice[T](elements: Set[(T, LamportClock)] = Set[(T, LamportClock)](),
                                clocks: VectorClock = VectorClock()) {

  def values(): Set[T] = elements.map(_._1)

  def removed(element: T): AddWinsSetLattice[T] = {
    copy(elements = elements.filterNot(_._1 == element))
  }

  def added(value: T, replicaId: String): AddWinsSetLattice[T] = {
    val clocksAfterAdd = clocks.advance(replicaId)
    val newLocalClock = clocksAfterAdd.clockOf(replicaId)
    val newElem = (value, newLocalClock)
    val elementsAfterAdd = elements.filterNot(_._1 == value) + newElem
    AddWinsSetLattice(elementsAfterAdd, clocksAfterAdd)
  }

}

object AddWinsSetLattice {
  // See: arXiv:1210.3368 (https://arxiv.org/pdf/1210.3368.pdf)
  implicit def AddWinsSetSemiLattice[T]: SemiLattice[AddWinsSetLattice[T]] =
    (left: AddWinsSetLattice[T], right: AddWinsSetLattice[T]) => {
      // If it's present in left and has a timestamp higher than highest timestamp of right, it was just added on left
      val presentInRight = right.values()
      val addedOnLeft = left.elements
        .filterNot { case (elem, _) => presentInRight.contains(elem) }
        .filter { case (_, LamportClock(c, i)) => c > right.clocks.timeOf(i) }

      // Vice versa
      val presentInLeft = left.values()
      val addedOnRight = right.elements
        .filterNot { case (elem, _) => presentInLeft.contains(elem) }
        .filter { case (_, LamportClock(c, i)) => c > left.clocks.timeOf(i) }

      // If it's present in both, it wasn't removed.
      // Clean up 'old' elements (keep only element with highest timestamp per replicaId)
      val presentInBoth = (left.elements & right.elements)
        .groupBy { case (value, LamportClock(_, rId)) => (value, rId) }
        .values
        .map(groupedElements => groupedElements.maxBy(_._2))
        .toSet

      val elementsAfterMerge = presentInBoth ++ addedOnLeft ++ addedOnRight
      val clocksAfterMerge = left.clocks.merged(right.clocks)

      AddWinsSetLattice(elementsAfterMerge, clocksAfterMerge)
    }
}