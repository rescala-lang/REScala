package lofi_acl.sync

import lofi_acl.crypto.PublicIdentity
import rdts.time.{Dot, Dots}

case class DeltaMapWithPrefix[RDT](prefixDots: Dots, prefix: RDT, deltaDots: Dots, deltas: Map[Dot, RDT]):
  lazy val allDots = prefixDots.union(deltaDots)

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
