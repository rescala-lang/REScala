package kofre.encrdt.crdts

import kofre.dotbased.DotStore.*
import kofre.causality.CausalContext
import kofre.dotbased.{WithContext, DotStore}

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object DeltaAddWinsMap {
  type DeltaAddWinsMapLattice[K, V] = WithContext[Map[K, V]]

  def empty[K, V: DotStore]: DeltaAddWinsMapLattice[K, V] = WithContext(Map.empty, CausalContext.empty)

  /** Returns the '''delta''' that contains the recursive mutation performed by the `deltaMutator`.
    *
    * @param key          Key of the element that is mutated
    * @param deltaMutator The delta-mutator that returns the delta of the recursive mutation
    * @param map          The map on which the delta-mutator is applied
    * @tparam K The type of the key
    * @tparam V The type of the value (needs to be a Delta CRDT)
    * @return The delta of the recursive delta-mutation
    */
  def deltaMutate[K, V: DotStore](
                                   key: K,
                                   deltaMutator: WithContext[V] => WithContext[V],
                                   map: DeltaAddWinsMapLattice[K, V]
  ): DeltaAddWinsMapLattice[K, V] = {

    deltaMutator(WithContext(
      map.store.getOrElse(key, DotStore[V].empty),
      map.context
      )) match {
      case WithContext(dotStore, causalContext) => WithContext(
          Map(key -> dotStore),
          causalContext
        )
    }
  }

  /** Returns the '''delta''' of the removal of the value associated to `key` from the `map`.
    *
    * @param key The key to remove
    * @param map The map on on which the removal of the mapping is performed
    * @tparam K The type of the key
    * @tparam V The type of the value (needs to be a Delta CRDT)
    * @return The delta that contains the removal (and nothing else)
    */
  def deltaRemove[K, V: DotStore](key: K, map: DeltaAddWinsMapLattice[K, V]): DeltaAddWinsMapLattice[K, V] =
    WithContext(
      Map.empty,
      map.store.get(key).map(DotStore[V].dots).getOrElse(CausalContext.empty)
      )

  /** Returns the '''delta''' that removes all values from the `map`.
    *
    * @param map The map on on which the removal of all mappings is performed
    * @tparam K The type of the key
    * @tparam V The type of the value (needs to be a Delta CRDT)
    * @return The delta that contains the removal of all mappings
    */
  def deltaClear[K, V: DotStore](map: DeltaAddWinsMapLattice[K, V]): DeltaAddWinsMapLattice[K, V] = WithContext(
    Map.empty,
    DotStore[Map[K, V]].dots(map.store)
    )
}
