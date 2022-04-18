package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.syntax.{OpsSyntaxHelper, QueryCtx}
import kofre.decompose.UIJDLattice.*

/** A GCounter is a Delta CRDT modeling an increment-only counter. */
object GCounterInterface {
  type GCounter = Map[String, Int]

  extension [C](container: C)
    def asGcounter: GCounter.GCounterSyntax[C] = GCounter.GCounterSyntax(container)

  object GCounter:
    implicit class GCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, GCounter](container) {
      def value(using QueryP): Int = current.valuesIterator.sum

      def inc()(using MutationIDP): C = Map(replicaID -> (current.getOrElse(replicaID, 0) + 1))
    }
}
