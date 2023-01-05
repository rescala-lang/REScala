package kofre.datatypes

import kofre.base.DecomposeLattice.*
import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.decompose.*
import kofre.dotted.DottedDecompose
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermQuery}

// this could be an opaque type, but interop
case class GrowOnlyCounter(inner: Map[Id, Int]) derives DecomposeLattice, Bottom

/** A GCounter is a Delta CRDT modeling an increment-only counter. */
object GrowOnlyCounter {
  def zero: GrowOnlyCounter = GrowOnlyCounter(Map.empty)

  given contextDecompose: DottedDecompose[GrowOnlyCounter] = DottedDecompose.liftDecomposeLattice

  implicit class GrowOnlyCounterSyntax[C](container: C)(using aoc: ArdtOpsContains[C, GrowOnlyCounter])
      extends OpsSyntaxHelper[C, GrowOnlyCounter](container) {
    def value(using QueryP): Int = current.inner.valuesIterator.sum

    def inc()(using MutationIdP): C =
      GrowOnlyCounter(Map(replicaID -> (current.inner.getOrElse(replicaID, 0) + 1))).mutator
    def inc(amount: Int)(using MutationIdP): C =
      require(amount >= 0, "may not decrease counter")
      GrowOnlyCounter(Map(replicaID -> (current.inner.getOrElse(replicaID, 0) + amount))).mutator
  }
}
