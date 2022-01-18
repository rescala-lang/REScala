package kofre.primitives

import kofre.Lattice
import kofre.IdUtil.Id

case class PosNegCounter(positiveCounts: Map[Id, Int], negativeCounts: Map[Id, Int]) {

  def updated(replicaId: Id, delta: Int): PosNegCounter = {
    if (delta > 0) this.copy(
      positiveCounts = positiveCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta))
    )
    else if (delta < 0) this.copy(
      negativeCounts = negativeCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta.abs))
    )
    else this
  }

  def query(): Int = positiveCounts.values.sum - negativeCounts.values.sum
}

object PosNegCounter {

  def zero: PosNegCounter = PosNegCounter(Map.empty, Map.empty)

  given lattice: Lattice[PosNegCounter] =
    given Lattice[Int] = math.max _
    Lattice.derived
}
