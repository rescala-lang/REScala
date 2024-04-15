package lofi_acl.ardt.datatypes

import lofi_acl.ardt.base.Bottom
import lofi_acl.ardt.util.MapHelper.max
import rdts.base.Lattice

case class Counter(positiveCounts: Map[String, Int] = Map.empty, negativeCounts: Map[String, Int] = Map.empty) {
  def value: Int = positiveCounts.values.sum - negativeCounts.values.sum
}

object Counter:
  val zero: Counter = Counter()

  given lattice: Lattice[Counter] = (left: Counter, right: Counter) =>
    Counter(
      positiveCounts = max(left.positiveCounts, right.positiveCounts),
      negativeCounts = max(left.negativeCounts, right.negativeCounts)
    )

  given bottom: Bottom[Counter] with
    override val empty: Counter = zero

  object mutators:
    def updated(counter: Counter, delta: Int, replicaId: String): Counter =
      if delta > 0 then
        counter.copy(positiveCounts = Map.Map1(replicaId, counter.positiveCounts.getOrElse(replicaId, 0) + delta))
      else if delta < 0 then
        counter.copy(negativeCounts = Map.Map1(replicaId, counter.negativeCounts.getOrElse(replicaId, 0) + delta))
      else Counter.zero
