package kofre.dotted

import kofre.base.Bottom
import kofre.time.Dots

case class DotMap[K, V](repr: Map[K, V])

/** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
  * [[kofre.datatypes.ObserveRemoveMap]] for a usage example.
  */
object DotMap {

  def empty[K, V]: DotMap[K, V] = DotMap(Map.empty)

  given hasDots[K, V: HasDots]: HasDots[DotMap[K, V]] with {
    override def getDots(a: DotMap[K, V]): Dots =
      a.repr.valuesIterator.foldLeft(Dots.empty)((acc, v) => acc.union(v.dots))
  }

  /** This essentially lifts the [[DottedLattice]] to a [[DotMap]].
    * Recursively merging values present in both maps with the given context.
    */
  given dottedLattice[K, V: DottedLattice: HasDots: Bottom]: DottedLattice[DotMap[K, V]] =
    new DottedLattice[DotMap[K, V]] {

      override def mergePartial(left: Dotted[DotMap[K, V]], right: Dotted[DotMap[K, V]]): DotMap[K, V] = {
        val empty = Bottom.empty[V]
        DotMap((left.store.repr.keySet union right.store.repr.keySet).iterator.flatMap { key =>
          val leftCausalStore  = left.map(_.repr.getOrElse(key, Bottom.empty[V]))
          val rightCausalStore = right.map(_.repr.getOrElse(key, Bottom.empty[V]))
          val res              = leftCausalStore mergePartial rightCausalStore
          if empty == res then None else Some(key -> res)
        }.toMap)
      }

      override def lteq(left: Dotted[DotMap[K, V]], right: Dotted[DotMap[K, V]]): Boolean = {
        if !(right.context <= left.context) then return false

        val empty = Bottom.empty[V]

        (left.store.repr.keySet union right.store.repr.keySet).forall { k =>
          left.map(_.repr.getOrElse(k, empty)) <= right.map(_.repr.getOrElse(k, empty))
        }
      }

      override def decompose(state: Dotted[DotMap[K, V]]): Iterable[Dotted[DotMap[K, V]]] = {
        val added = for {
          (k, v)                    <- state.store.repr
          Dotted(atomicV, atomicCC) <- Dotted(v, v.dots).decomposed
        } yield Dotted(DotMap(Map(k -> atomicV)), atomicCC)

        added ++ DottedDecompose.decomposedDeletions(state)
      }
    }
}
