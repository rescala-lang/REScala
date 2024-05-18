package lofi_acl.sync

import rdts.base.Bottom
import rdts.time.{Dot, Dots}

import scala.collection.mutable

class DeltaStore[RDT: Bottom] {
  private var prefixDots: Dots = Dots.empty
  private var prefix: RDT      = Bottom[RDT].empty

  private val deltaMap: mutable.Map[Dot, (Dots, RDT)] = mutable.Map.empty
  private var unionOfDots: Dots                       = Dots.empty

  def writePrefix(dots: Dots, delta: RDT): Unit =
    // Prune entries that are in the written prefix (dots) but not in the old prefix (prefixDots) that were written before (unionOfDots)
    prefixDots.intersect(dots).intersect(unionOfDots)
      .iterator.foreach { dot =>
        deltaMap.remove(dot)
      }
    // Store prefix
    prefix = delta
    prefixDots = dots
    unionOfDots = unionOfDots.union(dots)

  def writeIfNotPresent(dots: Dots, delta: RDT): Unit =
    if unionOfDots.contains(dots) then return
    val tuple = (dots, delta)
    dots.iterator.foreach { dot =>
      val _ = deltaMap.put(dot, tuple)
    }
    unionOfDots = unionOfDots.merge(dots)

  def readAvailableDeltas(dots: Dots): Seq[(Dots, RDT)] =
    var remaining = dots.intersect(unionOfDots) // Ensures that we only try to read available deltas

    var deltas = Seq.empty[(Dots, RDT)]

    if !prefixDots.intersect(dots).isEmpty then
      if prefixDots.contains(dots) then return Seq(prefixDots -> prefix)
      else
        deltas = Seq(prefixDots -> prefix)
        remaining = remaining.subtract(prefixDots)

    while (!remaining.isEmpty) {
      val dottedDelta @ (dots, _) = deltaMap(remaining.head)
      remaining = remaining.subtract(dots)
      deltas = dottedDelta +: deltas
    }
    deltas
}
