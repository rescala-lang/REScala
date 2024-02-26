package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice}
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.{OpsSyntaxHelper, ReplicaId}
import rdts.time.Dots
import rdts.dotted.HasDots.mapInstance

case class ObserveRemoveMap[K, V](inner: Map[K, V])

/** An ObserveRemoveMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GrowOnlyMap]], ObserveRemoveMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object ObserveRemoveMap {

  def empty[K, V]: ObserveRemoveMap[K, V] = ObserveRemoveMap(Map.empty)

  given bottom[K, V]: Bottom[ObserveRemoveMap[K, V]] = Bottom.derived

  given hasDots[K, V: HasDots]: HasDots[ObserveRemoveMap[K, V]] = HasDots.derived

  given contextDecompose[K, V: Lattice: HasDots: Bottom]: Lattice[ObserveRemoveMap[K, V]] =
    Lattice.derived

  private def make[K, V](
      dm: Map[K, V] = Map.empty[K, V],
      cc: Dots = Dots.empty
  ): Dotted[ObserveRemoveMap[K, V]] = Dotted(ObserveRemoveMap(dm), cc)

  extension [C, K, V](container: C)
    def observeRemoveMap: syntax[C, K, V] = syntax(container)

  implicit class syntax[C, K, V](container: C)
      extends OpsSyntaxHelper[C, ObserveRemoveMap[K, V]](container) {

    def contains(using IsQuery)(k: K): Boolean = current.inner.contains(k)

    def queryKey[A](using IsQuery, Bottom[V])(k: K): V = {
      current.inner.getOrElse(k, Bottom[V].empty)
    }

    def queryAllEntries(using IsQuery): Iterable[V] = current.inner.values
    def entries(using IsQuery): Iterable[(K, V)]    = current.inner.view

    def update(using ReplicaId, IsCausalMutator, Bottom[V])(k: K, v: V): C = {
      transform(k)(_ => Dotted(v, Dots.single(context.nextDot(replicaId))))
    }

    def transform(using
        pcm: IsCausalMutator,
        bot: Bottom[V]
    )(k: K)(m: Dotted[V] => Dotted[V]): C = {
      val v                           = current.inner.getOrElse(k, Bottom[V].empty)
      val Dotted(stateDelta, ccDelta) = m(Dotted(v, context))
      make[K, V](
        dm = Map(k -> stateDelta),
        cc = ccDelta
      ).mutator
    }

    def remove(using IsCausalMutator, HasDots[V])(k: K): C = {
      current.inner.get(k) match
        case Some(value) => make[K, V](
            cc = HasDots[V].dots(value)
          ).mutator
        case None => make[K, V]().mutator

    }

    def removeAll(using IsCausalMutator, Bottom[V], HasDots[V])(keys: Iterable[K]): C = {
      val values = keys.map(k => current.inner.getOrElse(k, Bottom[V].empty))
      val dots = values.foldLeft(Dots.empty) {
        case (set, v) => set union HasDots[V].dots(v)
      }

      make(
        cc = dots
      ).mutator
    }

    def removeByValue(using IsCausalMutator, HasDots[V])(cond: Dotted[V] => Boolean): C = {
      val toRemove = current.inner.values.collect {
        case v if cond(Dotted(v, context)) => v.dots
      }.fold(Dots.empty)(_ union _)

      make(
        cc = toRemove
      ).mutator
    }

    def clear(using IsCausalMutator, HasDots[V])(): C = {
      make(
        cc = current.inner.dots
      ).mutator
    }
  }
}
