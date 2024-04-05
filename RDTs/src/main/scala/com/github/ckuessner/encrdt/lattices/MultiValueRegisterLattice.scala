package com.github.ckuessner.encrdt.lattices

import com.github.ckuessner.encrdt.causality.VectorClock
import scala.annotation.tailrec

case class MultiValueRegisterLattice[T](versions: Map[VectorClock, T])

object MultiValueRegisterLattice {
  given MVRegLattice[T](using pOrd: PartialOrdering[VectorClock]): SemiLattice[MultiValueRegisterLattice[T]] =
    (left, right) => {
      val both   = left.versions ++ right.versions
      val toKeep = parallelVersionSubset(both.keySet.toList, List.empty)
      MultiValueRegisterLattice(both.filter { case (_, t) => toKeep.contains(t) })
    }

  @tailrec
  private def parallelVersionSubset[T](list: List[T], acc: List[T])(using pOrd: PartialOrdering[T]): List[T] =
    list match {
      case head :: Nil => head :: acc
      case head :: tail =>
        val newTailWithComp = tail
          .map(other => other -> pOrd.tryCompare(head, other))
          .filter {
            case (_, None)       => true
            case (_, Some(comp)) => comp < 0 // head smaller < tail => tail contains head
          }

        val headIsContainedInTail = newTailWithComp.exists {
          case (_, Some(comp)) => comp < 0
          case _               => false
        }

        var newAcc = acc
        if (!headIsContainedInTail) {
          newAcc = head :: acc
        }

        parallelVersionSubset(newTailWithComp.map(_._1), newAcc)
      case Nil => acc
    }
}
