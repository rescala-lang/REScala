package encrdtlib.lattices

import kofre.base.Bottom
import kofre.dotted.HasDots.*
import kofre.dotted.{DotFun, DotMap, Dotted, HasDots}
import kofre.time.Dots

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object DeltaAddWinsMap {
  type DeltaAddWinsMapLattice[K, V] = Dotted[DotMap[K, V]]

  def empty[K, V: HasDots]: DeltaAddWinsMapLattice[K, V] = Dotted(DotMap.empty, Dots.empty)

  /** Returns the '''delta''' that contains the recursive mutation performed by the `deltaMutator`.
    *
    * @param key          Key of the element that is mutated
    * @param deltaMutator The delta-mutator that returns the delta of the recursive mutation
    * @param map          The map on which the delta-mutator is applied
    * @tparam K The type of the key
    * @tparam V The type of the value (needs to be a Delta CRDT)
    * @return The delta of the recursive delta-mutation
    */
  def deltaMutate[K, V: HasDots](
      key: K,
      default: => V,
      deltaMutator: Dotted[V] => Dotted[V],
      map: DeltaAddWinsMapLattice[K, V]
  ): DeltaAddWinsMapLattice[K, V] = {

    deltaMutator(Dotted(
      map.store.getOrElse(key, default),
      map.context
    )) match {
      case Dotted(dotStore, causalContext) => Dotted(
          DotMap(Map(key -> dotStore)),
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
  def deltaRemove[K, V: HasDots](key: K, map: DeltaAddWinsMapLattice[K, V]): DeltaAddWinsMapLattice[K, V] =
    Dotted(
      DotMap.empty,
      map.store.get(key).map(HasDots[V].dots).getOrElse(Dots.empty)
    )

  /** Returns the '''delta''' that removes all values from the `map`.
    *
    * @param map The map on on which the removal of all mappings is performed
    * @tparam K The type of the key
    * @tparam V The type of the value (needs to be a Delta CRDT)
    * @return The delta that contains the removal of all mappings
    */
  def deltaClear[K, V: HasDots](map: DeltaAddWinsMapLattice[K, V]): DeltaAddWinsMapLattice[K, V] = Dotted(
    DotMap.empty,
    map.store.dots
  )
}
