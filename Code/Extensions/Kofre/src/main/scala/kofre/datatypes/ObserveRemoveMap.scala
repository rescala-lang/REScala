package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.datatypes.{GrowOnlyMap, ObserveRemoveMap}
import kofre.dotted.{DotMap, Dotted, DottedDecompose, DottedLattice, HasDots}
import kofre.syntax.{DottedName, OpsSyntaxHelper}
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

  given contextDecompose[K, V: DottedDecompose: HasDots: Bottom]: DottedDecompose[ObserveRemoveMap[K, V]] =
    import DotMap.contextDecompose
    DottedDecompose.derived

  private def make[K, V](
      dm: DotMap[K, V] = DotMap.empty[K, V],
      cc: Dots = Dots.empty
  ): Dotted[ObserveRemoveMap[K, V]] = Dotted(ObserveRemoveMap(dm), cc)

  extension [C, K, V](container: C)
    def observeRemoveMap: syntax[C, K, V] = syntax(container)

  implicit class syntax[C, K, V](container: C)
      extends OpsSyntaxHelper[C, ObserveRemoveMap[K, V]](container) {

    def contains(using QueryP)(k: K): Boolean = current.contains(k)

    def queryKey[A](using QueryP, CausalP, Bottom[V])(k: K): Dotted[V] = {
      Dotted(current.inner.getOrElse(k, Bottom[V].empty), context)
    }

    def queryAllEntries(using QueryP): Iterable[V] = current.inner.values
    def mutateKey(k: K, m: (Id, Dotted[V]) => Dotted[V])(using
        CausalMutationP,
        IdentifierP,
        Bottom[V]
    ): C = {
      val v = current.inner.getOrElse(k, Bottom[V].empty)

      m(replicaID, context.wrap(v)) match {
        case Dotted(stateDelta, ccDelta) =>
          make[K, V](
            dm = DotMap(Map(k -> stateDelta)),
            cc = ccDelta
          ).mutator
      }
    }

    def mutateKeyNamedCtx(using
        CausalMutationP,
        IdentifierP,
        Bottom[V]
    )(k: K)(m: DottedName[V] => DottedName[V]): C = {
      val v                           = current.inner.getOrElse(k, Bottom[V].empty)
      val Dotted(stateDelta, ccDelta) = m(DottedName(replicaID, Dotted(v, context))).anon
      make[K, V](
        dm = DotMap(Map(k -> stateDelta)),
        cc = ccDelta
      ).mutator
    }

    def remove(using CausalMutationP, Bottom[V], HasDots[V])(k: K): C = {
      val v = current.inner.getOrElse(k, Bottom[V].empty)

      make[K, V](
        cc = HasDots[V].dots(v)
      ).mutator
    }

    def removeAll(using CausalMutationP, Bottom[V], HasDots[V])(keys: Iterable[K]): C = {
      val values = keys.map(k => current.inner.getOrElse(k, Bottom[V].empty))
      val dots = values.foldLeft(Dots.empty) {
        case (set, v) => set union HasDots[V].dots(v)
      }

      make(
        cc = dots
      ).mutator
    }

    def removeByValue(using CausalMutationP, DottedDecompose[V], HasDots[V])(cond: Dotted[V] => Boolean): C = {
      val toRemove = current.inner.values.collect {
        case v if cond(Dotted(v, context)) => HasDots[V].dots(v)
      }.fold(Dots.empty)(_ union _)

      make(
        cc = toRemove
      ).mutator
    }

    def clear(using CausalMutationP, DottedDecompose[V], HasDots[V])(): C = {
      make(
        cc = current.inner.dots
      ).mutator
    }
  }
}
