package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.HasDots.mapInstance
import rdts.dotted.{Dotted, HasDots, Obrem}
import rdts.time.Dots

case class ObserveRemoveMap[K, V](inner: Map[K, V]) {
  export inner.{get}

  import ObserveRemoveMap.make

  type Delta = Obrem[ObserveRemoveMap[K, V]]

  def contains(k: K): Boolean = inner.contains(k)

  def queryKey[A](using Bottom[V])(k: K): V = {
    inner.getOrElse(k, Bottom[V].empty)
  }

  def queryAllEntries: Iterable[V] = inner.values

  def entries: Iterable[(K, V)] = inner.view

  def update(using LocalUid)(k: K, v: V)(using context: Dots): Delta = {
    Obrem(ObserveRemoveMap(Map(k -> v)), Dots.single(context.nextDot(LocalUid.replicaId)), Dots.empty)
  }

  def transformPlain(using LocalUid)(k: K)(m: Option[V] => Option[V])(using context: Dots): Delta = {
    m(inner.get(k)) match {
      case Some(value) => update(k, value)
      case None        => Obrem(ObserveRemoveMap.empty, Dots.single(context.nextDot(LocalUid.replicaId)), Dots.empty)
    }
  }

  def transform(using
      bot: Bottom[V],
      hd: HasDots[V]
  )(k: K)(m: Dotted[V] => Dotted[V])(using context: Dots): Delta = {
    val v   = inner.getOrElse(k, Bottom[V].empty)
    val res = m(Dotted(v, context))

    Obrem(
      ObserveRemoveMap(Map(k -> res.data)),
      observed = res.contained,
      deletions = res.deletions
    )
  }

  def remove(using HasDots[V])(k: K): Delta = {
    inner.get(k) match
      case Some(value) =>
        Obrem(
          ObserveRemoveMap.empty,
          observed = Dots.empty,
          deletions = HasDots[V].dots(value)
        )
      case None => Obrem(ObserveRemoveMap.empty)

  }

  def removeAll(using Bottom[V], HasDots[V])(keys: Iterable[K]): Delta = {
    val values = keys.map(k => inner.getOrElse(k, Bottom[V].empty))
    val dots = values.foldLeft(Dots.empty) {
      case (set, v) => set `union` HasDots[V].dots(v)
    }

    Obrem(
      ObserveRemoveMap.empty,
      observed = Dots.empty,
      deletions = dots
    )
  }

  def removeByValue(using HasDots[V])(cond: Dotted[V] => Boolean)(using context: Dots): Delta = {
    val toRemove = inner.values.collect {
      case v if cond(Dotted(v, context)) => v.dots
    }.fold(Dots.empty)(_ `union` _)

    Obrem(
      ObserveRemoveMap.empty,
      observed = Dots.empty,
      deletions = toRemove
    )
  }

  def clear(using HasDots[V])(): Delta = {
    Obrem(
      ObserveRemoveMap.empty,
      observed = Dots.empty,
      deletions = inner.dots
    )
  }
}

/** An ObserveRemoveMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GrowOnlyMap]], ObserveRemoveMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object ObserveRemoveMap {

  case class Entry[V](dots: Dots, value: V)
  object Entry {
    given bottom[V: Bottom]: Bottom[Entry[V]]    = Bottom.derived
    given lattice[V: Lattice]: Lattice[Entry[V]] = Lattice.derived

    given hasDots[V]: HasDots[Entry[V]] = new HasDots[Entry[V]] {
      extension (dotted: Entry[V]) {
        override def dots: Dots = dotted.dots

        override def removeDots(dots: Dots): Option[Entry[V]] =
          val res = dotted.dots `subtract` dots
          Option.when(!res.isEmpty):
            Entry(res, dotted.value)
      }
    }
  }

  def empty[K, V]: ObserveRemoveMap[K, V] = ObserveRemoveMap(Map.empty)

  given bottom[K, V]: Bottom[ObserveRemoveMap[K, V]] = Bottom.derived

  given hasDots[K, V: HasDots]: HasDots[ObserveRemoveMap[K, V]] = HasDots.derived

  given lattice[K, V: Lattice: HasDots]: Lattice[ObserveRemoveMap[K, V]] =
    Lattice.derived

  private def make[K, V](
      dm: Map[K, V] = Map.empty[K, V],
      cc: Dots = Dots.empty
  ): Dotted[ObserveRemoveMap[K, V]] = Dotted(ObserveRemoveMap(dm), cc)

}
