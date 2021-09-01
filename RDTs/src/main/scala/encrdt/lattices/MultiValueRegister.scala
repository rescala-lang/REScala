package de.ckuessner
package encrdt.lattices

import encrdt.causality.VectorClock
import encrdt.lattices.interfaces.SemiLattice

import scala.annotation.tailrec

class MultiValueRegister[T](initialState: MultiValueRegisterLattice[T], val replicaId: String) {
  private var _state = initialState

  def currentTime: VectorClock = {
    if (state.versions.isEmpty) VectorClock()
    else state.versions.keys.reduce((a, b) => a.merged(b))
  }

  def state: MultiValueRegisterLattice[T] = _state

  def values: List[T] = state.versions.values.toList

  def set(value: T): Unit = {
    val timeOfUpdate = currentTime.advance(replicaId)
    _state = MultiValueRegisterLattice(Map(timeOfUpdate->value))
  }

  def merge(otherState: MultiValueRegisterLattice[T]): Unit =
    _state = SemiLattice.merged(this.state, otherState)
}

case class MultiValueRegisterLattice[T](versions: Map[VectorClock, T])

object MultiValueRegisterLattice {
  implicit def MVRegLattice[T](implicit pOrd: PartialOrdering[VectorClock]): SemiLattice[MultiValueRegisterLattice[T]] =
    (left, right) => {
      val both = left.versions ++ right.versions
      val toKeep = parallelVersionSubset(both.keySet.toList, List.empty)
      MultiValueRegisterLattice(both.filter { case (_, t) => toKeep.contains(t) })
    }

  @tailrec
  private def parallelVersionSubset[T](list: List[T], acc: List[T])
                                      (implicit pOrd: PartialOrdering[T]): List[T] =
    list match {
      case head :: Nil => head :: acc
      case head :: tail =>
        val newTailWithComp = tail
          .map(other => other -> pOrd.tryCompare(head, other))
          .filter {
            case (_, None) => true
            case (_, Some(comp)) => comp < 0 // head smaller < tail => tail contains head
          }

        val headIsContainedInTail = newTailWithComp.exists {
          case (_, Some(comp)) => comp < 0
          case _ => false
        }

        var newAcc = acc
        if (!headIsContainedInTail) {
          newAcc = head :: acc
        }

        parallelVersionSubset(newTailWithComp.map(_._1), newAcc)
      case Nil => acc
    }
}
