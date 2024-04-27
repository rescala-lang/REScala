package rdts.datatypes

import rdts.base.{Bottom, Lattice}
import rdts.dotted.HasDots
import rdts.syntax.OpsSyntaxHelper

case class PosNegCounter(pos: GrowOnlyCounter, neg: GrowOnlyCounter) derives Lattice, Bottom

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GrowOnlyCounter]]) to enable both increments and decrements of the counter value.
  */
object PosNegCounter {

  val zero: PosNegCounter = PosNegCounter(GrowOnlyCounter.zero, GrowOnlyCounter.zero)

  given hasDots: HasDots[PosNegCounter] = HasDots.noDots

  extension [C](container: C)
    def posNegCounter: syntax[C] = syntax(container)

  implicit class syntax[C](container: C)
      extends OpsSyntaxHelper[C, PosNegCounter](container) {
    def value(using IsQuery): Int =
      val pos = current.pos.value
      val neg = current.neg.value
      pos - neg

    def inc(): IdMutator =
      val pos = current.pos.inc()
      PosNegCounter(pos, GrowOnlyCounter.zero).mutator

    def dec(): IdMutator =
      val neg = current.neg.inc()
      PosNegCounter(GrowOnlyCounter.zero, neg).mutator

    def add(delta: Int): IdMutator = {
      if (delta > 0) PosNegCounter(current.pos.add(delta), GrowOnlyCounter.zero)
      else if (delta < 0) PosNegCounter(GrowOnlyCounter.zero, current.neg.add(-delta))
      else PosNegCounter.zero
    }.mutator
  }
}
