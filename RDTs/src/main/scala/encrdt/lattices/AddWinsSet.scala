package de.ckuessner
package encrdt.lattices

import encrdt.causality.{LamportClock, VectorClock}
import encrdt.lattices.interfaces.{SemiLattice, SetCrdt}

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

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
    copy(elements = elements.filter(_._1 == element))
  }

  def added(newElem: T, replicaId: String): AddWinsSetLattice[T] = {
    // Prepare
    val clocksAfterAdd = clocks.advance(replicaId)
    val newLocalClock = clocks.clockOf(replicaId)

    // Effect
    val elementsAfterAdd = (elements + ((newElem, newLocalClock))) filterNot {
      case (elem, elemClock) =>
        elem == newElem && elemClock.replicaId == replicaId && elemClock.time < newLocalClock.time
    }

    AddWinsSetLattice(elementsAfterAdd, clocksAfterAdd)
  }

}

object AddWinsSetLattice {
  // See: arXiv:1210.3368 (https://arxiv.org/pdf/1210.3368.pdf)
  implicit def AddWinsSetSemiLattice[T]: SemiLattice[AddWinsSetLattice[T]] =
    (left: AddWinsSetLattice[T], right: AddWinsSetLattice[T]) => {
      // If it's present in both, it wasn't removed
      val presentInBoth = left.elements & right.elements

      // If it's present on left and has a timestamp higher than highest timestamp of right, it was just added on left
      val addedOnLeft = (left.elements -- right.elements) filter { case (_, LamportClock(c, i)) => c > right.clocks.timeOf(i) }
      // Vice versa
      val addedOnRight = (right.elements -- left.elements) filter { case (_, LamportClock(c, i)) => c > left.clocks.timeOf(i) }

      val allElements = presentInBoth ++ addedOnLeft ++ addedOnRight
      // Clean up 'old' elements (keep only element with only highest timestamp per replicaId)
      val elementsAfterMerge = allElements.groupBy { case (e, LamportClock(_, i)) => (e, i) }.view.mapValues(_.maxBy(_._2))

      val clocksAfterMerge = left.clocks.merged(right.clocks)

      AddWinsSetLattice(elementsAfterMerge.values.toSet, clocksAfterMerge)
    }

  implicit def codec[T](implicit jsonValueCodec: JsonValueCodec[T]): JsonValueCodec[AddWinsSetLattice[T]] =
    JsonCodecMaker.make[AddWinsSetLattice[T]]
}