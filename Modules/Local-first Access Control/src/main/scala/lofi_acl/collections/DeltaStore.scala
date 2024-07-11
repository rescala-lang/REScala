package lofi_acl.collections

import lofi_acl.collections.DeltaStore
import rdts.base.{Bottom, Lattice}
import rdts.time.{Dot, Dots}

/** Stores a larger state (i.e., the `merge`of multiple deltas, called the prefix) along with individual deltas.
  *
  * Each individual delta group is addressed by a single `Dot` and is stored alongside the Dots of the delta group.
  * The value addressableDeltas stores the keys (key refers to a single Dot) of the stored delta groups. Note that while
  * each Dot in addressableDeltas is part of the Dots stored along the delta group, the reverse is not true. I.e., not
  * every Dot part of the Dots stored alongside a delta is an address for a delta.
  * The rationale behind this is that we want to keep a merged prefix and prune the deltas that we likely won't retrieve
  * individually. This is useful for a scenario, where we know which deltas are already delivered to every known replica
  * that we might need to send this delta to.
  *
  * @param prefixDots The dots of the prefix.
  * @param prefix The prefix delta.
  * @param addressableDeltas The keys (dots) of stored deltas. (Not necessarily the same as the `union` of dots of the delta groups, but a subset).
  * @param deltas The delta groups addressed by a Dot.
  * @tparam RDT The type of the deltas.
  */
case class DeltaStore[RDT] private /* private to ensure invariant (see from(…) in companion object) */ (
    prefixDots: Dots,
    prefix: RDT,
    addressableDeltas: Dots, // Note that there might be delta groups with dots that are not addressable but stored
    deltas: Map[Dot, (Dots, RDT)]
):
  lazy val retrievableDots: Dots = prefixDots.union(addressableDeltas)

  def contains(dots: Dots): Boolean = {
    retrievableDots.contains(dots)
  }

  def readAvailableDeltas(dots: Dots): List[(Dots, RDT)] = {
    var remaining = dots.intersect(retrievableDots) // Ensures that we only try to read available deltas

    // Check if some of the dots are only present in the prefix. If so, add prefix to result.
    var collectedDeltas = if !addressableDeltas.contains(remaining) then
      remaining = remaining.subtract(prefixDots)
      List((prefixDots, prefix))
    else
      List.empty[(Dots, RDT)]

    while !remaining.isEmpty do {
      val dottedDelta @ (dotsOfDelta, _) = deltas(remaining.head)
      remaining = remaining.subtract(dotsOfDelta)
      collectedDeltas = dottedDelta :: collectedDeltas
    }

    collectedDeltas
  }

  def readAvailableDeltasAsSingleDelta(dots: Dots)(using lattice: Lattice[RDT]): Option[(Dots, RDT)] = {
    readAvailableDeltas(dots).reduceOption {
      case ((lDots, lDelta), (rDots, rDelta)) =>
        (lDots.union(rDots), lattice.merge(lDelta, rDelta))
    }
  }

  def replacePrefixPruningDeltas(dots: Dots, delta: RDT): DeltaStore[RDT] = {
    // Prune entries that are in the written prefix (dots) and addressable (addressableDeltas)
    val toPrune      = dots.intersect(addressableDeltas)
    val prunedDeltas = deltas.removedAll(toPrune.iterator)
    DeltaStore(
      prefixDots = dots,
      prefix = delta,
      addressableDeltas = addressableDeltas.subtract(dots),
      deltas = prunedDeltas
    )
  }

  def mergeIntoPrefixPruningDeltas(dots: Dots, delta: RDT)(using lattice: Lattice[RDT]): DeltaStore[RDT] = {
    val toPrune      = dots.intersect(addressableDeltas)
    val prunedDeltas = deltas.removedAll(toPrune.iterator)
    DeltaStore(
      prefixDots = prefixDots.union(dots),
      prefix = lattice.merge(prefix, delta),
      addressableDeltas = addressableDeltas.subtract(dots),
      deltas = prunedDeltas
    )
  }

  def replaceOrMergeIntoPrefixPruningDeltas(dots: Dots, delta: RDT)(using Lattice[RDT]): DeltaStore[RDT] = {
    if !dots.contains(prefixDots)
    then mergeIntoPrefixPruningDeltas(dots, delta)
    else replacePrefixPruningDeltas(dots, delta)
  }

  def addDeltaEvenIfRedundant(dots: Dots, delta: RDT): DeltaStore[RDT] = {
    val dottedDelta = dots -> delta
    copy(
      addressableDeltas = addressableDeltas.union(dots),
      deltas = deltas ++ dots.iterator.map(dot => dot -> dottedDelta)
    )
  }

  def addDeltaOnlyAtDot(dot: Dot, dots: Dots, delta: RDT): DeltaStore[RDT] = {
    require(dots.contains(dot))
    copy(
      addressableDeltas = addressableDeltas.add(dot),
      deltas = deltas + (dot -> (dots -> delta))
    )
  }

  def addDeltaIfNew(dots: Dots, delta: RDT): DeltaStore[RDT] = {
    val dotsToStore = dots.subtract(prefixDots).subtract(addressableDeltas)
    if dotsToStore.isEmpty then return this
    val dottedDelta = dots -> delta
    copy(
      addressableDeltas = addressableDeltas.union(dotsToStore),
      deltas = deltas ++ dotsToStore.iterator.map(dot => dot -> dottedDelta)
    )
  }

  def discardDeltasContainedInPrefix: DeltaStore[RDT] = {
    val toPrune = addressableDeltas.intersect(prefixDots)
    copy(
      addressableDeltas = addressableDeltas.subtract(toPrune),
      deltas = deltas.removedAll(toPrune.iterator)
    )
  }

  /** Creates a new DeltaStore that merges the deltas into the prefix and discards the deltas.
    *
    * This implementation doesn't `merge`deltas already part of the prefix (according their dot address) or are duplicate
    * entries in the delta map (according to their dot address).
    *
    * @return A DeltaStore with the result of the `merge`of the old prefix and all irredundant deltas and no additional
    *         deltas.
    */
  def compactAllDeltasIntoPrefixSkippingDuplicates(using
      lattice: Lattice[RDT],
      bottom: Bottom[RDT]
  ): DeltaStore[RDT] = {
    if deltas.isEmpty then return this

    var newPrefixDots = prefixDots
    var mergedDeltas  = bottom.empty
    val deltaIterator = deltas.iterator
    while deltaIterator.hasNext do {
      val (dot, (dots, delta)) = deltaIterator.next()
      if !newPrefixDots.contains(dot) then
        newPrefixDots = newPrefixDots.union(dots)
        mergedDeltas = lattice.merge(mergedDeltas, delta)
    }

    DeltaStore(
      newPrefixDots,
      lattice.merge(prefix, mergedDeltas),
      Dots.empty,
      Map.empty
    )
  }

  def compactAllDeltasIntoPrefix(using lattice: Lattice[RDT]): DeltaStore[RDT] = {
    if deltas.isEmpty then return this

    val (mergedDots, mergedDeltas) = deltas.values.reduce {
      case ((dotsLeft, deltaLeft), (dotsRight, deltaRight)) =>
        (dotsLeft.union(dotsRight), lattice.merge(deltaLeft, deltaRight))
    }
    DeltaStore(
      prefixDots = prefixDots.union(mergedDots),
      prefix = lattice.merge(prefix, mergedDeltas),
      addressableDeltas = Dots.empty,
      deltas = Map.empty
    )
  }

object DeltaStore:
  def empty[RDT: Bottom](): DeltaStore[RDT] =
    DeltaStore(Dots.empty, Bottom[RDT].empty, Dots.empty, Map.empty)

  def from[RDT](
      prefixDots: Dots,
      prefix: RDT,
      addressableDeltas: Dots,
      deltas: Map[Dot, (Dots, RDT)]
  ): DeltaStore[RDT] = {
    require(
      addressableDeltas.iterator.forall(dot => deltas.contains(dot)),
      "all addressableDeltas must have a dot in deltas"
    )
    DeltaStore(prefixDots, prefix, addressableDeltas, deltas)
  }
