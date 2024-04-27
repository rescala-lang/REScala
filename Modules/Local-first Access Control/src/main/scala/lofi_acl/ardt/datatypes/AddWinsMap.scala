package lofi_acl.ardt.datatypes

import lofi_acl.ardt.base.{Bottom, Causal}
import lofi_acl.ardt.causality.DotStore
import lofi_acl.ardt.causality.DotStore.{DotFun, DotMap}
import rdts.base.Lattice
import rdts.syntax.LocalUid

import java.time.Instant
import scala.language.implicitConversions

opaque type AddWinsMap[K, V]               = Causal[DotMap[K, V]]
opaque type AddWinsLastWriterWinsMap[K, V] = AddWinsMap[K, DotFun[(V, (Instant, String))]]

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
object AddWinsMap:
  extension [K, V](self: AddWinsMap[K, V]) def get(key: K): Option[V] = self.dotStore.get(key)

  given bottom[K, V: DotStore]: Bottom[AddWinsMap[K, V]] with
    override val empty: AddWinsMap[K, V] = Causal.bottom[DotMap[K, V]]

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
    private[datatypes] def mutate[K, V: DotStore](
        map: AddWinsMap[K, V],
        key: K,
        deltaMutator: Causal[V] => Causal[V]
    ): AddWinsMap[K, V] = {

      deltaMutator(Causal(map.dotStore.getOrElse(key, DotStore[V].bottom), map.causalContext)) match {
        case Causal(dotStore, causalContext) =>
          Causal(
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
    def remove[K, V: DotStore](key: K, map: AddWinsMap[K, V]): AddWinsMap[K, V] = Causal(
      DotStore[DotMap[K, V]].bottom,
      DotStore[V].dots(map.dotStore.getOrElse(key, DotStore[V].bottom))
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
    def clear[K, V: DotStore](map: AddWinsMap[K, V]): AddWinsMap[K, V] = Causal(
      DotStore[DotMap[K, V]].bottom,
      DotStore[DotMap[K, V]].dots(map.dotStore)
    )

object AddWinsLastWriterWinsMap {
  private given lwwLattice: Lattice[(Instant, String)] = (left, right) =>
    if (left == right) left
    else if (left._1.isAfter(right._1)) left
    else if (right._1.isAfter(left._1)) right
    else if (left._2 > right._2) left
    else if (right._2 > left._2) right
    else throw new IllegalArgumentException()

  private given timestampedValueLattice[V]: Lattice[(V, (Instant, String))] = (left, right) => {
    if (Lattice.merge(left._2, right._2) == left._2) left
    else right
  }

  extension [K, V](self: AddWinsLastWriterWinsMap[K, V])
    def get(key: K): Option[V] =
      val dotMap: Map[K, DotFun[(V, (Instant, String))]] = self.dotStore
      dotMap.get(key) match
        case Some(dotFun) =>
          dotFun.maxByOption { case (_, (_, dot: (Instant, String))) => dot }
            .map { case (_, (v: V, _)) => v }
        case None => None

    def values: Map[K, V] =
      self.dotStore.map { case (k, mvReg) =>
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

    def remove[K, V](map: AddWinsLastWriterWinsMap[K, V], key: K): AddWinsMap[K, DotFun[(V, (Instant, String))]] =
      AddWinsMap.mutators.remove(key, map)
}
