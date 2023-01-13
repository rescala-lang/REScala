package kofre.datatypes

import kofre.base.DecomposeLattice.*
import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.dotted.DottedDecompose
import kofre.syntax.{OpsSyntaxHelper, PermQuery}

case class GrowOnlyCounter(inner: Map[Id, Int]) derives DecomposeLattice, Bottom

/** A GCounter is a Delta CRDT modeling an increment-only counter. */
object GrowOnlyCounter {
  def zero: GrowOnlyCounter = GrowOnlyCounter(Map.empty)

  given contextDecompose: DottedDecompose[GrowOnlyCounter] = DottedDecompose.liftDecomposeLattice

  extension [C](container: C)
    def growOnlyCounter: syntax[C] = syntax(container)

  implicit class syntax[C](container: C)
      extends OpsSyntaxHelper[C, GrowOnlyCounter](container) {
    def value(using PermQuery): Int = current.inner.valuesIterator.sum

    def inc(using PermIdMutate)(): C =
      GrowOnlyCounter(Map(replicaId -> (current.inner.getOrElse(replicaId, 0) + 1))).mutator
    def add(using PermIdMutate)(amount: Int): C =
      require(amount >= 0, "may not decrease counter")
      GrowOnlyCounter(Map(replicaId -> (current.inner.getOrElse(replicaId, 0) + amount))).mutator
  }
}
