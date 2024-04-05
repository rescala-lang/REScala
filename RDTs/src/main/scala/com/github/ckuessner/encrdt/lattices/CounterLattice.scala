package com.github.ckuessner.encrdt.lattices

import com.github.ckuessner.encrdt.util.MapHelper.max

case class CounterLattice(positiveCounts: Map[String, Int] = Map(), negativeCounts: Map[String, Int] = Map()) {

  def updated(replicaId: String, delta: Int): CounterLattice = {
    if (delta > 0)
      this.copy(
        positiveCounts = positiveCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta))
      )
    else if (delta < 0)
      this.copy(
        negativeCounts = negativeCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta.abs))
      )
    else this
  }

  def query(): Int = positiveCounts.values.sum - negativeCounts.values.sum
}

object CounterLattice {
  given semiLattice: SemiLattice[CounterLattice] = (left: CounterLattice, right: CounterLattice) =>
    CounterLattice(
      max(left.positiveCounts, right.positiveCounts),
      max(left.negativeCounts, right.negativeCounts)
    )
}
