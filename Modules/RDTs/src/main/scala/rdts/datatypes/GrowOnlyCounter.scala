package rdts.datatypes

import rdts.base.Lattice.*
import rdts.base.{Bottom, Lattice, Uid}
import rdts.dotted.HasDots
import rdts.syntax.{OpsSyntaxHelper, PermQuery}

case class GrowOnlyCounter(inner: Map[Uid, Int]) derives Bottom

/** A GCounter is a Delta CRDT modeling an increment-only counter. */
object GrowOnlyCounter {
  def zero: GrowOnlyCounter = GrowOnlyCounter(Map.empty)

  given hasDots: HasDots[GrowOnlyCounter] = HasDots.noDots

  given lattice: Lattice[GrowOnlyCounter] =
    given Lattice[Int] = math.max
    Lattice.derived

  extension [C](container: C)
    def growOnlyCounter: syntax[C] = syntax(container)

  implicit class syntax[C](container: C)
      extends OpsSyntaxHelper[C, GrowOnlyCounter](container) {
    def value(using IsQuery): Int = current.inner.valuesIterator.sum

    def inc(): IdMutator =
      GrowOnlyCounter(Map(replicaId -> (current.inner.getOrElse(replicaId, 0) + 1))).mutator
    def add(amount: Int): IdMutator =
      require(amount >= 0, "may not decrease counter")
      GrowOnlyCounter(Map(replicaId -> (current.inner.getOrElse(replicaId, 0) + amount))).mutator
  }
}
