package kofre.predef

import kofre.base.DecomposeLattice.*
import kofre.base.{DecomposeLattice, Defs}
import kofre.decompose.*
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermQuery}

// this could be an opaque type, but interop
case class GrowOnlyCounter(inner: Map[Defs.Id, Int]) derives DecomposeLattice

/** A GCounter is a Delta CRDT modeling an increment-only counter. */
object GrowOnlyCounter {
  def zero: GrowOnlyCounter = GrowOnlyCounter(Map.empty)

  implicit class GrowOnlyCounterSyntax[C](container: C)(using aoc: ArdtOpsContains[C, GrowOnlyCounter]) extends OpsSyntaxHelper[C, GrowOnlyCounter](container) {
    def value(using QueryP): Int = current.inner.valuesIterator.sum

    def inc()(using MutationIDP): C = GrowOnlyCounter(Map(replicaID -> (current.inner.getOrElse(replicaID, 0) + 1)))
    def inc(amount: Int)(using MutationIDP): C =
      require(amount >= 0, "may not decrease counter")
      GrowOnlyCounter(Map(replicaID -> (current.inner.getOrElse(replicaID, 0) + amount)))
  }
}
