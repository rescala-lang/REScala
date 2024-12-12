package lofi_acl.ardt.datatypes

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.Dot

import java.time.Instant
import scala.language.implicitConversions

opaque type AddWinsMap[K, V]               = Dotted[Map[K, V]]
opaque type AddWinsLastWriterWinsMap[K, V] = AddWinsMap[K, Map[Dot, (V, (Instant, String))]]

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object AddWinsMap:
  extension [K, V](self: AddWinsMap[K, V]) def get(key: K): Option[V] = self.data.get(key)

  given bottom[K, V: Bottom]: Bottom[AddWinsMap[K, V]] with
    override val empty: AddWinsMap[K, V] = Dotted.empty

  object mutators:
    /** Returns the '''delta''' that contains the recursive mutation performed by the `deltaMutator`.
      *
      * @param key
      *   Key of the element that is mutated
      * @param deltaMutator
      *   The delta-mutator that returns the delta of the recursive mutation
      * @param map
      *   The map on which the delta-mutator is applied
      * @tparam K
      *   The type of the key
      * @tparam V
      *   The type of the value (needs to be a Delta CRDT)
      * @return
      *   The delta of the recursive delta-mutation
      */
    private[datatypes] def mutate[K, V: Bottom](
        map: AddWinsMap[K, V],
        key: K,
        deltaMutator: Dotted[V] => Dotted[V]
    ): AddWinsMap[K, V] = {

      deltaMutator(Dotted(map.data.getOrElse(key, Bottom[V].empty), map.context)) match {
        case Dotted(dotStore, causalContext) =>
          Dotted(
            Map(key -> dotStore),
            causalContext
          )
      }
    }

    /** Returns the '''delta''' of the removal of the value associated to `key` from the `map`.
      *
      * @param key
      *   The key to remove
      * @param map
      *   The map on on which the removal of the mapping is performed
      * @tparam K
      *   The type of the key
      * @tparam V
      *   The type of the value (needs to be a Delta CRDT)
      * @return
      *   The delta that contains the removal (and nothing else)
      */
    def remove[K, V: {Bottom, HasDots}](key: K, map: AddWinsMap[K, V]): AddWinsMap[K, V] = Dotted(
      Bottom[Map[K, V]].empty,
      HasDots[V].dots(map.data.getOrElse(key, Bottom[V].empty))
    )

    /** Returns the '''delta''' that removes all values from the `map`.
      *
      * @param map
      *   The map on on which the removal of all mappings is performed
      * @tparam K
      *   The type of the key
      * @tparam V
      *   The type of the value (needs to be a Delta CRDT)
      * @return
      *   The delta that contains the removal of all mappings
      */
    def clear[K, V: Bottom](map: AddWinsMap[K, V]): AddWinsMap[K, V] = Dotted(
      Bottom[Map[K, V]].empty,
      HasDots[Map[K, V]].dots(map.data)
    )

object AddWinsLastWriterWinsMap {
  private given lwwLattice: Lattice[(Instant, String)] = (left, right) =>
    if left == right then left
    else if left._1.isAfter(right._1) then left
    else if right._1.isAfter(left._1) then right
    else if left._2 > right._2 then left
    else if right._2 > left._2 then right
    else throw new IllegalArgumentException()

  private given timestampedValueLattice[V]: Lattice[(V, (Instant, String))] = (left, right) => {
    if Lattice.merge(left._2, right._2) == left._2 then left
    else right
  }

  extension [K, V](self: AddWinsLastWriterWinsMap[K, V])
    def get(key: K): Option[V] =
      val dotMap: Map[K, Map[Dot, (V, (Instant, String))]] = self.data
      dotMap.get(key) match
        case Some(dotFun) =>
          dotFun.maxByOption { case (_, (_, dot: (Instant, String))) => dot }
            .map { case (_, (v: V, _)) => v }
        case None => None

    def values: Map[K, V] =
      self.data.map { case (k, mvReg) =>
        k -> mvReg.values.maxBy(_._2)._1
      }

  object mutators:
    def put[K, V](
        map: AddWinsLastWriterWinsMap[K, V],
        key: K,
        value: V,
        replicaId: LocalUid
    ): AddWinsLastWriterWinsMap[K, V] = {
      AddWinsMap.mutators.mutate(
        map,
        key,
        MultiValueRegister.mutators.write(_, (value, (Instant.now(), replicaId.uid.delegate)), replicaId)
      )
    }

    def remove[K, V](map: AddWinsLastWriterWinsMap[K, V], key: K): AddWinsMap[K, Map[Dot, (V, (Instant, String))]] =
      AddWinsMap.mutators.remove(key, map)
}
