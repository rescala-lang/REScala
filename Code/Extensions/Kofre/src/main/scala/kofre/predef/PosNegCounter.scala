package kofre.predef

import kofre.base.DecomposeLattice
import kofre.base.Defs.Id
import kofre.syntax.PermIdMutate.withID
import kofre.syntax.{PermIdMutate, ArdtOpsContains, FixedId, PermId, OpsSyntaxHelper, PermQuery}

case class PosNegCounter(pos: GrowOnlyCounter, neg: GrowOnlyCounter) derives DecomposeLattice

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GCounterInterface]]) to enable both increments and decrements of the counter value.
  */
object PosNegCounter {

  val zero: PosNegCounter = PosNegCounter(GrowOnlyCounter.zero, GrowOnlyCounter.zero)

  implicit class PNCounterSyntax[C](container: C)(using ArdtOpsContains[C, PosNegCounter])
      extends OpsSyntaxHelper[C, PosNegCounter](container) {
    def value(using QueryP): Int =
      val pos = current._1.value
      val neg = current._2.value
      pos - neg

    def inc()(using MutationIdP): C =
      val pos = current._1.inc()(using withID(replicaID))
      PosNegCounter(pos, GrowOnlyCounter.zero).mutator

    def dec()(using MutationIdP): C =
      val neg = current._2.inc()(using withID(replicaID))
      PosNegCounter(GrowOnlyCounter.zero, neg).mutator

    def add(delta: Int)(using MutationIdP): C = {
      if (delta > 0) PosNegCounter(current.pos.inc(delta)(using withID(replicaID)), GrowOnlyCounter.zero)
      else if (delta < 0) PosNegCounter(GrowOnlyCounter.zero, current.neg.inc(-delta)(using withID(replicaID)))
      else PosNegCounter.zero
    }.mutator
  }
}
