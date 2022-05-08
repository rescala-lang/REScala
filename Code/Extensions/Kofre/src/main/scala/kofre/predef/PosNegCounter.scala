package kofre.predef

import kofre.base.DecomposeLattice
import kofre.base.Defs.Id
import kofre.decompose.interfaces.GCounterInterface
import kofre.decompose.interfaces.GCounterInterface.{GCounter, asGcounter}
import kofre.syntax.AllPermissionsCtx.withID
import kofre.syntax.{AllPermissionsCtx, FixedIdCtx, IdentifierCtx, OpsSyntaxHelper, QueryCtx}


case class PosNegCounter(pos: GCounter, neg: GCounter) derives DecomposeLattice

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GCounterInterface]]) to enable both increments and decrements of the counter value.
  */
object PosNegCounter {
  implicit class PNCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, PosNegCounter](container) {
    def value(using QueryP): Int =
      val pos = current._1.asGcounter.value
      val neg = current._2.asGcounter.value
      pos - neg

    def inc()(using MutationIDP): C =
      val pos = current._1.asGcounter.inc()(using withID(replicaID))
      PosNegCounter(pos, Map.empty)

    def dec()(using MutationIDP): C =
      val neg = current._2.asGcounter.inc()(using withID(replicaID))
      PosNegCounter(Map.empty, neg)
  }
}
