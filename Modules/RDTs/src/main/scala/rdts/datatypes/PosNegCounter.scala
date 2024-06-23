package rdts.datatypes

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.HasDots

case class PosNegCounter(pos: GrowOnlyCounter, neg: GrowOnlyCounter) derives Lattice, Bottom {
  def value: Int =
    val posv = pos.value
    val negv = neg.value
    posv - negv

  def inc()(using LocalUid): PosNegCounter = add(1)

  def dec()(using LocalUid): PosNegCounter = add(-1)

  def add(delta: Int)(using LocalUid): PosNegCounter = {
    if delta > 0 then PosNegCounter(pos.add(delta), GrowOnlyCounter.zero)
    else if delta < 0 then PosNegCounter(GrowOnlyCounter.zero, neg.add(-delta))
    else PosNegCounter.zero
  }
}

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GrowOnlyCounter]]) to enable both increments and decrements of the counter value.
  */
object PosNegCounter {

  val zero: PosNegCounter = PosNegCounter(GrowOnlyCounter.zero, GrowOnlyCounter.zero)

  given hasDots: HasDots[PosNegCounter] = HasDots.noDots
}
