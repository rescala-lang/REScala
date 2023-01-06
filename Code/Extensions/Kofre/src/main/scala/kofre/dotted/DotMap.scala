package kofre.dotted

import kofre.base.Bottom
import kofre.time.Dots
import kofre.dotted.DottedDecompose.FromConlattice
import kofre.decompose.interfaces

case class DotMap[K, V](repr: Map[K, V]) {
  def dots(using ccv: HasDots[V]): Dots =
    repr.valuesIterator.foldLeft(Dots.empty)((acc, v) => acc.union(ccv.dots(v)))
  export repr.{repr as _, *}
}

/** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
  * [[kofre.datatypes.ObserveRemoveMap]] for a usage example.
  */
object DotMap {

  def empty[K, V]: DotMap[K, V] = DotMap(Map.empty)

  given hasDots[K, V: HasDots]: HasDots[DotMap[K, V]] with {
    override def dots(a: DotMap[K, V]): Dots = a.dots

  }

  /** This essentially lifts the [[DottedLattice]] to a [[DotMap]].
    * Recursively merging values present in both maps with the given context.
    */
  given dottedLattice[K, V: DottedLattice: Bottom]: DottedLattice[DotMap[K, V]] with {
    override def mergePartial(left: Dotted[DotMap[K, V]], right: Dotted[DotMap[K, V]]): DotMap[K, V] = {
      DotMap((left.store.repr.keySet union right.store.repr.keySet).flatMap { key =>
        val leftCausalStore  = left.map(_.getOrElse(key, Bottom.empty[V]))
        val rightCausalStore = right.map(_.getOrElse(key, Bottom.empty[V]))
        val res              = leftCausalStore mergePartial rightCausalStore
        if Bottom.empty[V] == res then None else Some(key -> res)
      }.toMap)
    }
  }

  given contextDecompose[K, V: DottedDecompose: HasDots: Bottom]: DottedDecompose[DotMap[K, V]] =
    new FromConlattice[DotMap[K, V]](dottedLattice) {

      override def lteq(left: Dotted[DotMap[K, V]], right: Dotted[DotMap[K, V]]): Boolean = {
        def firstCondition = (left.context subtract right.context).isEmpty

        def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
          left.map(_.getOrElse(k, Bottom.empty[V])) <= right.map(_.getOrElse(k, Bottom.empty[V]))
        }

        def secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

        firstCondition && secondCondition
      }

      override def decompose(state: Dotted[DotMap[K, V]]): Iterable[Dotted[DotMap[K, V]]] = {
        val added = for {
          k <- state.store.keys
          Dotted(atomicV, atomicCC) <- {
            val v = state.store.getOrElse(k, Bottom.empty[V])
            DottedDecompose[V].decompose(Dotted(v, HasDots[V].dots(v)))
          }
        } yield Dotted(DotMap(Map(k -> atomicV)), atomicCC)

        val removed =
          state.context.subtract(state.store.dots).decomposed.map(Dotted(
            DotMap.empty[K, V],
            _
          ))

        added ++ removed
      }
    }
}
