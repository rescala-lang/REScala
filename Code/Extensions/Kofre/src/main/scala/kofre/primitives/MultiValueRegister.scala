package kofre.primitives

import kofre.IdUtil.Id
import kofre.Lattice
import kofre.Lattice.Operators
import kofre.primitives.VectorClock

import scala.annotation.tailrec

/** Keeps all concurrent writes */
case class MultiValueRegister[T](versions: Map[VectorClock, T]) {
  lazy val currentTime: VectorClock = {
    if (versions.isEmpty) VectorClock.zero
    else versions.keys.reduce((a, b) => a.merge(b))
  }

  def values: Iterable[T] = versions.values

  def write(replica: Id, value: T): MultiValueRegister[T] = {
    val timeOfUpdate = currentTime merge currentTime.inc(replica)
    MultiValueRegister(Map(timeOfUpdate -> value))
  }
}

object MultiValueRegister {
  given lattice[T]: Lattice[MultiValueRegister[T]] =
    (left, right) => {
      val both   = left.versions ++ right.versions
      val toKeep = parallelVersionSubset(both.keySet.toList, List.empty)
      MultiValueRegister(both.filter { case (k, v) => toKeep.contains(k) })
    }

  @tailrec
  private def parallelVersionSubset(list: List[VectorClock], acc: List[VectorClock]): List[VectorClock] =
    list match {
      case head :: Nil => head :: acc
      case head :: tail =>
        val newTailWithComp = tail
          .map(other => other -> head.tryCompare(other))
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
