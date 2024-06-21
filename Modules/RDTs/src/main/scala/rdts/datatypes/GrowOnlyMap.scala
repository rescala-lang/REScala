package rdts.datatypes

import rdts.base.{Bottom, Lattice}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.Dots

/** A GMap (Grow-only Map) is a Delta CRDT that models a map from an arbitrary key type to nested Delta CRDTs.
  * In contrast to [[rdts.datatypes.contextual.ObserveRemoveMap]], key/value pairs cannot be removed from this map. However, due to the smaller internal
  * representation, mutate operations on large maps are a lot faster than on ObserveRemoveMap.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
case class GrowOnlyMap[K, V](inner: Map[K, V]) {

  export inner.{get, getOrElse, contains, values, valuesIterator, keySet, keys, keysIterator, exists, iterator}

  def mutateKeyNamedCtx(k: K, default: => V)(m: Dotted[V] => Dotted[V])(using
      context: Dots
  ): Dotted[GrowOnlyMap[K, V]] = {
    m(
      context.wrap(inner.getOrElse(k, default))
    ).map(v => GrowOnlyMap(Map(k -> v)))
  }
}

object GrowOnlyMap {

  def empty[K, V]: GrowOnlyMap[K, V] = GrowOnlyMap(Map.empty)

  given bottom[K, V]: Bottom[GrowOnlyMap[K, V]] = Bottom.provide(empty)

  given lattice[K, V: Lattice]: Lattice[GrowOnlyMap[K, V]] = Lattice.derived
  given hasDots[K, V]: HasDots[GrowOnlyMap[K, V]]          = HasDots.derived

}
