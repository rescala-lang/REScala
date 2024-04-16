package lofi_acl.ardt.datatypes

import rdts.base.{Bottom, Lattice}

case class TwoPhaseMap[K, V](
    keys: TwoPhaseSet[K] = TwoPhaseSet.empty[K],
    private[ardt] val _mappings: Map[K, V] = Map.empty[K, V]
):
  def get(key: K): Option[V] =
    if keys.contains(key)
    then _mappings.get(key)
    else None

object TwoPhaseMap:
  def empty[K, V]: TwoPhaseMap[K, V] = TwoPhaseMap[K, V]()

  import TwoPhaseSet.lattice
  import lofi_acl.ardt.base.StandardLibrary.GrowOnlyMap.lattice

  given lattice[K, V: Lattice]: Lattice[TwoPhaseMap[K, V]] =
    case (TwoPhaseMap(leftKeys, leftMappings), TwoPhaseMap(rightKeys, rightMappings)) =>
      TwoPhaseMap(Lattice.merge(leftKeys, rightKeys), Lattice.merge(leftMappings, rightMappings))

  given bottom[K, V]: Bottom[TwoPhaseMap[K, V]] with
    override val empty: TwoPhaseMap[K, V] = TwoPhaseMap.empty

  object mutators:
    def put[K, V](twoPhaseMap: TwoPhaseMap[K, V], key: K, value: V): TwoPhaseMap[K, V] =
      if twoPhaseMap.keys.removed.contains(key) then TwoPhaseMap.empty
      // Could also send merged result -> Depending on use-case this might result in lower or higher delta size
      else TwoPhaseMap(TwoPhaseSet(added = Set(key)), Map(key -> value))

    def remove[K, V](twoPhaseMap: TwoPhaseMap[K, V], keyToRemove: K): TwoPhaseMap[K, V] =
      TwoPhaseMap(keys = TwoPhaseSet(removed = Set(keyToRemove)))
