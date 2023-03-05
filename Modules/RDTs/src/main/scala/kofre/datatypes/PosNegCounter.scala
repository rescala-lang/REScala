package kofre.datatypes

import kofre.base.{Bottom, Lattice, Uid}
import kofre.dotted.DottedLattice
import kofre.syntax.OpsSyntaxHelper

case class PosNegCounter(pos: GrowOnlyCounter, neg: GrowOnlyCounter) derives Lattice, Bottom

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GrowOnlyCounter]]) to enable both increments and decrements of the counter value.
  */
object PosNegCounter {

  val zero: PosNegCounter = PosNegCounter(GrowOnlyCounter.zero, GrowOnlyCounter.zero)

  given contextDecompose: DottedLattice[PosNegCounter] = DottedLattice.liftLattice

  extension [C](container: C)
    def posNegCounter: syntax[C] = syntax(container)

  implicit class syntax[C](container: C)
      extends OpsSyntaxHelper[C, PosNegCounter](container) {
    def value(using PermQuery): Int =
      val pos = current.pos.growOnlyCounter.value
      val neg = current.neg.growOnlyCounter.value
      pos - neg

    def inc(): IdMutate =
      val pos = current.pos.growOnlyCounter.inc()
      PosNegCounter(pos, GrowOnlyCounter.zero).mutator

    def dec(): IdMutate =
      val neg = current.neg.growOnlyCounter.inc()
      PosNegCounter(GrowOnlyCounter.zero, neg).mutator

    def add(delta: Int): IdMutate = {
      if (delta > 0) PosNegCounter(current.pos.growOnlyCounter.add(delta), GrowOnlyCounter.zero)
      else if (delta < 0) PosNegCounter(GrowOnlyCounter.zero, current.neg.growOnlyCounter.add(-delta))
      else PosNegCounter.zero
    }.mutator
  }
}
