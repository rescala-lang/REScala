package com.github.ckuessner.encrdt.lattices

case class TwoPhaseMapLattice[K, V: SemiLattice](keys: TwoPhaseSetLattice[K] = TwoPhaseSetLattice[K](),
                                                 mappings: Map[K, V] = Map[K, V]()) {

  def values: Map[K, V] = mappings

  def get(key: K): Option[V] =
    if (keys.contains(key)) mappings.get(key)
    else None

  def added(key: K, value: V): TwoPhaseMapLattice[K, V] = {
    // Was key previously removed? If yes, adding it won't change the outcome
    if (keys.removedElems.contains(key)) return this
    // Is there a conflicting value?
    // There should be at most one (at least if key->??? ist not in entries.removed)
    get(key) match {
      case Some(conflictingValue) =>
        // Merge value with value currently in map
        mappings + (key -> SemiLattice.merged(value, conflictingValue))
        TwoPhaseMapLattice(keys, mappings)
      case None =>
        TwoPhaseMapLattice(
          keys.added(key),
          mappings + (key -> value)
        )
    }
  }

  def removed(key: K): TwoPhaseMapLattice[K, V] = copy(
    keys = keys.removed(key),
    // This requires pruning in merge (otherwise the deleted values will be added again on merge)
    mappings = mappings.filterNot { case (k, v) => k == key }
  )
}

object TwoPhaseMapLattice {

  import OptionLattice.optLattice

  def twoPhaseMapLattice[K, V: SemiLattice]: SemiLattice[TwoPhaseMapLattice[K, V]] = (l, r) => {
    val mergedKeys = SemiLattice.merged(l.keys, r.keys)
    val mergedMap = mergedKeys.values.map { (key: K) =>
      key -> SemiLattice.merged(l.get(key), r.get(key)).get
    }.toMap

    TwoPhaseMapLattice(
      mergedKeys,
      mergedMap
    )
  }
}
