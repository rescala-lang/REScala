package rdts.datatypes

import rdts.base.Lattice.*
import rdts.base.{Bottom, Lattice, Uid}
import rdts.dotted.HasDots
import rdts.syntax.{LocalUid, OpsSyntaxHelper, PermQuery}

case class GrowOnlyCounter(inner: Map[Uid, Int]) {
  lazy val value: Int = inner.valuesIterator.sum

  def inc()(using localReplicaId: LocalUid): GrowOnlyCounter = add(1)
  def add(amount: Int)(using localReplicaId: LocalUid): GrowOnlyCounter =
    require(amount >= 0, "may not decrease counter")
    GrowOnlyCounter(Map(localReplicaId.uid -> (inner.getOrElse(localReplicaId.uid, 0) + amount)))
}

/** A GCounter is a Delta CRDT modeling an increment-only counter. */
object GrowOnlyCounter {
  def zero: GrowOnlyCounter = GrowOnlyCounter(Map.empty)

  given hasDots: HasDots[GrowOnlyCounter] = HasDots.noDots

  given bottom: Bottom[GrowOnlyCounter] = Bottom.derived

  given lattice: Lattice[GrowOnlyCounter] =
    given Lattice[Int] = math.max
    Lattice.derived

}
