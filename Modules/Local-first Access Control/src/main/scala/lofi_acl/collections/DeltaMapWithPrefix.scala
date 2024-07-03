package lofi_acl.collections

import rdts.base.Bottom
import rdts.time.{Dot, Dots}

case class DeltaMapWithPrefix[RDT](prefixDots: Dots, prefix: RDT, deltaDots: Dots, deltas: Map[Dot, RDT]):
  lazy val allDots: Dots = prefixDots.union(deltaDots)

  def contains(dots: Dots): Boolean =
    allDots.contains(dots)

  def addDelta(dot: Dot, delta: RDT): DeltaMapWithPrefix[RDT] = copy(
    deltaDots = deltaDots.add(dot),
    deltas = deltas + (dot -> delta)
  )

  def retrieveDeltas(dots: Dots): Iterator[(Dot, RDT)] =
    dots
      .intersect(deltaDots) // Make sure to only try to retrieve contained deltas
      .iterator
      .map(dot => dot -> deltas(dot))

  def dropDeltas(dotsToRemove: Dots): DeltaMapWithPrefix[RDT] =
    copy(
      deltaDots = deltaDots.subtract(dotsToRemove),
      deltas = deltas.filterNot((dot, _) => dotsToRemove.contains(dot))
    )

object DeltaMapWithPrefix {
  def empty[RDT: Bottom]: DeltaMapWithPrefix[RDT] =
    DeltaMapWithPrefix(Dots.empty, Bottom[RDT].empty, Dots.empty, Map.empty)
}
