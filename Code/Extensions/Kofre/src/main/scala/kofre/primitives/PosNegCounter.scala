package kofre.primitives

import kofre.base.Defs.Id
import kofre.base.Lattice

case class PosNegCounter(positiveCounts: Map[Id, Int], negativeCounts: Map[Id, Int]) {

  def add(replicaId: Id, delta: Int): PosNegCounter = {
    if (delta > 0) PosNegCounter(Map(replicaId -> (positiveCounts.getOrElse(replicaId, 0) + delta)), Map.empty)
    else if (delta < 0) PosNegCounter(Map.empty, Map(replicaId -> (positiveCounts.getOrElse(replicaId, 0) - delta)))
    else PosNegCounter.zero
  }

  def value: Int = positiveCounts.values.sum - negativeCounts.values.sum
}

object PosNegCounter {

  val zero: PosNegCounter = PosNegCounter(Map.empty, Map.empty)

  given lattice: Lattice[PosNegCounter] =
    given Lattice[Int] = math.max _
    Lattice.derived
}
