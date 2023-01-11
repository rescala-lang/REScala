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
    def value(using QueryP): Int = current.inner.valuesIterator.sum

    def inc(using MutationIdP)(): C =
      GrowOnlyCounter(Map(replicaID -> (current.inner.getOrElse(replicaID, 0) + 1))).mutator
    def add(using MutationIdP)(amount: Int): C =
      require(amount >= 0, "may not decrease counter")
      GrowOnlyCounter(Map(replicaID -> (current.inner.getOrElse(replicaID, 0) + amount))).mutator
  }
}
