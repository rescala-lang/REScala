package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.SetCrdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

class AddWinsSet[T](val replicaId: Int) extends SetCrdt[T] {

  private var _state: AddWinsSetLattice[T] = AddWinsSetLattice[T]()

  def this(replicaId: Int, initialState: AddWinsSetLattice[T]) {
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

// insertions: Set[(element, time, replicaId)]
case class AddWinsSetLattice[T](elements: Set[(T, Int, Int)] = Set[(T, Int, Int)](),
                                clocks: Map[Int, Int] = Map[Int, Int]()) {

  def values(): Set[T] = elements.map(_._1)

  def removed(element: T): AddWinsSetLattice[T] = {
    copy(elements = elements.filter(_._1 == element))
  }

  def added(newElem: T, replicaId: Int): AddWinsSetLattice[T] = {
    // Prepare
    val newTime = clocks.getOrElse(replicaId, 1)
    val clocksAfterAdd = clocks + (replicaId -> newTime)
    val elementEntry = (newElem, newTime, replicaId)

    // Effect
    val elementsAfterAdd = (elements + elementEntry) filterNot {
      case (elem, clockOfElem, replicaOfElem) =>
        elem == newElem && replicaOfElem == replicaId && clockOfElem < newTime
    }

    AddWinsSetLattice(elementsAfterAdd, clocksAfterAdd)
  }

}

object AddWinsSetLattice {
  // See: arXiv:1210.3368
  implicit def AddWinsSetSemiLattice[T]: SemiLattice[AddWinsSetLattice[T]] =
    (left: AddWinsSetLattice[T], right: AddWinsSetLattice[T]) => {
      // If it's present in both, it wasn't removed
      val presentInBoth = left.elements & right.elements

      // If it's present on left and has a timestamp higher than highest timestamp of right, it was just added on left
      val addedOnLeft = (left.elements -- right.elements) filter { case (_, c, i) => c > right.clocks(i) }
      // Vice versa
      val addedOnRight = (right.elements -- left.elements) filter { case (_, c, i) => c > left.clocks(i) }

      val allElements = presentInBoth ++ addedOnLeft ++ addedOnRight
      // Clean up 'old' elements (keep only element with only highest timestamp per replicaId)
      val elementsAfterMerge = allElements.groupBy { case (e, _, i) => (e, i) }.view.mapValues(_.maxBy(_._2))

      val clocksAfterMerge = (left.clocks.keySet ++ right.clocks.keySet)
        .map(key => key -> left.clocks.getOrElse(key, 0).max(right.clocks.getOrElse(key, 0)))
        .toMap

      AddWinsSetLattice(elementsAfterMerge.values.toSet, clocksAfterMerge)
    }

  implicit def codec[T](implicit jsonValueCodec: JsonValueCodec[T]): JsonValueCodec[AddWinsSetLattice[T]] =
    JsonCodecMaker.make[AddWinsSetLattice[T]]
}