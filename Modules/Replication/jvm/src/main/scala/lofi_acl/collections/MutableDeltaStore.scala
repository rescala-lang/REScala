package lofi_acl.collections

import rdts.base.{Bottom, Lattice}
import rdts.time.{Dot, Dots}

import scala.collection.mutable

class MutableDeltaStore[RDT: Bottom] {
  private var prefixDots: Dots = Dots.empty
  private var prefix: RDT      = Bottom[RDT].empty

  private val deltaMap: mutable.Map[Dot, (Dots, RDT)] = mutable.Map.empty
  private var unionOfDots: Dots                       = Dots.empty

  def storedDots: Dots = unionOfDots

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

  def writePrefixIfContainsPreviousPrefix(dots: Dots, delta: RDT): Boolean =
    if !dots.contains(prefixDots) then false
    else
      writePrefix(dots, delta)
      true

  def mergeIntoPrefix(dots: Dots, delta: RDT)(using Lattice[RDT]): Unit =
    val mergedPrefix = prefix.merge(delta)
    val mergedDots   = prefixDots.merge(dots)

    unionOfDots.intersect(dots).removeDots(prefixDots) match
      case Some(toRemove) => toRemove.iterator.foreach { dot =>
          deltaMap.remove(dot)
        }
      case None =>

    prefix = mergedPrefix
    prefixDots = mergedDots
    unionOfDots = unionOfDots.union(dots)

  def writeIfNotPresent(dots: Dots, delta: RDT): Unit =
    val tuple       = (dots, delta)
    val missingDots = dots.subtract(unionOfDots)
    missingDots.iterator.foreach { dot =>
      val _ = deltaMap.put(dot, tuple)
    }
    unionOfDots = unionOfDots.merge(missingDots)

  def readAvailableDeltas(dots: Dots): List[(Dots, RDT)] =
    var remaining = dots.intersect(unionOfDots) // Ensures that we only try to read available deltas

    var deltas = List.empty[(Dots, RDT)]

    // Prefix has overlap with dots
    if !prefixDots.intersect(dots).isEmpty then
      if prefixDots.contains(dots) then return List(prefixDots -> prefix)
      else
        deltas = List(prefixDots -> prefix)
        remaining = remaining.subtract(prefixDots)

    while !remaining.isEmpty do {
      val dottedDelta @ (dots, _) = deltaMap(remaining.head)
      remaining = remaining.subtract(dots)
      deltas = dottedDelta :: deltas
    }
    deltas
}
