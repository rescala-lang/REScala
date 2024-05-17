package lofi_acl.sync

import rdts.time.{Dot, Dots}

import scala.collection.mutable

class DeltaStore[RDT] {
  // TODO: Could be optimized to only keep a common prefix and the deltas that might not have been seen by everyone
  private val deltaMap: mutable.Map[Dot, (Dots, RDT)] = mutable.Map.empty
  private var unionOfDots: Dots                       = Dots.empty

  def write(dots: Dots, delta: RDT): Unit =
    val tuple = (dots, delta)
    dots.iterator.foreach { dot =>
      val _ = deltaMap.put(dot, tuple)
    }
    unionOfDots = unionOfDots.merge(dots)

  def readAvailableDeltas(dots: Dots): Seq[(Dots, RDT)] =
    var remaining = dots.intersect(unionOfDots) // Ensures that we only try to read available deltas
    val retrieved = Dots.empty
    var deltas    = Seq.empty[(Dots, RDT)]
    while (!remaining.isEmpty) {
      val dottedDelta @ (dots, _) = deltaMap(remaining.head)
      remaining = remaining.intersect(dots)
      deltas = dottedDelta +: deltas
    }
    deltas
}
