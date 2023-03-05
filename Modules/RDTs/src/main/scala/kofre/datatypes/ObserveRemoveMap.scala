package kofre.datatypes

import kofre.base.{Bottom, Uid, Lattice}
import kofre.datatypes.{GrowOnlyMap, ObserveRemoveMap}
import kofre.dotted.{DotMap, Dotted, DottedLattice, HasDots}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}

case class ObserveRemoveMap[K, V](inner: DotMap[K, V])

/** An ObserveRemoveMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GrowOnlyMap]], ObserveRemoveMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object ObserveRemoveMap {

  def empty[K, V]: ObserveRemoveMap[K, V] = ObserveRemoveMap(DotMap.empty)

  given bottom[K, V]: Bottom[ObserveRemoveMap[K, V]] = Bottom.derived

  given hasDots[K, V: HasDots]: HasDots[ObserveRemoveMap[K, V]] = DotMap.hasDots[K, V].map(_.inner)

  given contextDecompose[K, V: DottedLattice: HasDots: Bottom]: DottedLattice[ObserveRemoveMap[K, V]] =
    DottedLattice.derived

  private def make[K, V](
      dm: DotMap[K, V] = DotMap.empty[K, V],
      cc: Dots = Dots.empty
  ): Dotted[ObserveRemoveMap[K, V]] = Dotted(ObserveRemoveMap(dm), cc)

  extension [C, K, V](container: C)
    def observeRemoveMap: syntax[C, K, V] = syntax(container)

  implicit class syntax[C, K, V](container: C)
      extends OpsSyntaxHelper[C, ObserveRemoveMap[K, V]](container) {

    def contains(using PermQuery)(k: K): Boolean = current.contains(k)

    def queryKey[A](using PermQuery, Bottom[V])(k: K): V = {
      current.inner.repr.getOrElse(k, Bottom[V].empty)
    }

    def queryAllEntries(using PermQuery): Iterable[V] = current.inner.repr.values
    def entries(using PermQuery): Iterable[(K, V)]    = current.inner.repr.view

    def insert(using ReplicaId, PermCausalMutate, Bottom[V])(k: K, v: V): C = {
      mutateKey(k)(dotted => Dotted(v, Dots.single(context.nextDot(replicaId))))
    }

    def mutateKey(using
        pcm: PermCausalMutate,
        bot: Bottom[V]
    )(k: K)(m: Dotted[V] => Dotted[V]): C = {
      val v                           = current.inner.repr.getOrElse(k, Bottom[V].empty)
      val Dotted(stateDelta, ccDelta) = m(Dotted(v, context))
      make[K, V](
        dm = DotMap(Map(k -> stateDelta)),
        cc = ccDelta
      ).mutator
    }

    def remove(using PermCausalMutate, Bottom[V], HasDots[V])(k: K): C = {
      val v = current.inner.repr.getOrElse(k, Bottom[V].empty)

      make[K, V](
        cc = HasDots[V].getDots(v)
      ).mutator
    }

    def removeAll(using PermCausalMutate, Bottom[V], HasDots[V])(keys: Iterable[K]): C = {
      val values = keys.map(k => current.inner.repr.getOrElse(k, Bottom[V].empty))
      val dots = values.foldLeft(Dots.empty) {
        case (set, v) => set union HasDots[V].getDots(v)
      }

      make(
        cc = dots
      ).mutator
    }

    def removeByValue(using PermCausalMutate, DottedLattice[V], HasDots[V])(cond: Dotted[V] => Boolean): C = {
      val toRemove = current.inner.repr.values.collect {
        case v if cond(Dotted(v, context)) => HasDots[V].getDots(v)
      }.fold(Dots.empty)(_ union _)

      make(
        cc = toRemove
      ).mutator
    }

    def clear(using PermCausalMutate, HasDots[V])(): C = {
      make(
        cc = current.inner.dots
      ).mutator
    }
  }
}
