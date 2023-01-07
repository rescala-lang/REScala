package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.dotted.DottedDecompose
import kofre.syntax.OpsSyntaxHelper

case class PosNegCounter(pos: GrowOnlyCounter, neg: GrowOnlyCounter) derives DecomposeLattice, Bottom

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GrowOnlyCounter]]) to enable both increments and decrements of the counter value.
  */
object PosNegCounter {

  val zero: PosNegCounter = PosNegCounter(GrowOnlyCounter.zero, GrowOnlyCounter.zero)

  given contextDecompose: DottedDecompose[PosNegCounter] = DottedDecompose.liftDecomposeLattice

  extension [C](container: C)
    def posNegCounter: syntax[C] = syntax(container)

  implicit class syntax[C](container: C)
      extends OpsSyntaxHelper[C, PosNegCounter](container) {
    def value(using QueryP): Int =
      val pos = current.pos.growOnlyCounter.value
      val neg = current.neg.growOnlyCounter.value
      pos - neg

    def inc(using MutationIdP)(): C =
      val pos = current.pos.growOnlyCounter.inc()
      PosNegCounter(pos, GrowOnlyCounter.zero).mutator

    def dec(using MutationIdP)(): C =
      val neg = current.neg.growOnlyCounter.inc()
      PosNegCounter(GrowOnlyCounter.zero, neg).mutator

    def add(using MutationIdP)(delta: Int): C = {
      if (delta > 0) PosNegCounter(current.pos.growOnlyCounter.add(delta), GrowOnlyCounter.zero)
      else if (delta < 0) PosNegCounter(GrowOnlyCounter.zero, current.neg.growOnlyCounter.add(-delta))
      else PosNegCounter.zero
    }.mutator
  }
}
