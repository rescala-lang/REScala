package kofre.dotted

import kofre.base.Bottom
import kofre.time.Dots

/** DotMap is just a container to use maps in a dotted context.
  * Merge/<= are done per entry, with missing entries replaced by `Bottom.empty`.
  * Decompose decomposes all components.
  *
  * It would be perfectly reasonable to skip the case class and define the instance on maps directly,
  * but that might cause issues with implicit resolution
  *
  * See [[kofre.datatypes.ObserveRemoveMap]] for a usage example.
  */
case class DotMap[K, V](repr: Map[K, V])

object DotMap {

  def empty[K, V]: DotMap[K, V] = DotMap(Map.empty)

  given hasDots[K, V: HasDots]: HasDots[DotMap[K, V]] with {
    override def getDots(a: DotMap[K, V]): Dots =
      a.repr.valuesIterator.map(v => v.dots).reduceOption(_ union _).getOrElse(Dots.empty)
  }

  given dottedLattice[K, V: DottedLattice: HasDots: Bottom]: DottedLattice[DotMap[K, V]] with {
    lazy val empty: V = Bottom.empty[V]

    def access(key: K)(m: DotMap[K, V]): V = m.repr.getOrElse(key, empty)

    override def mergePartial(left: Dotted[DotMap[K, V]], right: Dotted[DotMap[K, V]]): DotMap[K, V] = {
      val keys = left.store.repr.keySet union right.store.repr.keySet
      val inner =
        for
          key <- keys.iterator
          merged = left.map(access(key)) mergePartial right.map(access(key))
          if empty != merged
        yield key -> merged
      DotMap(inner.toMap)
    }

    override def lteq(left: Dotted[DotMap[K, V]], right: Dotted[DotMap[K, V]]): Boolean = {
      (right.context <= left.context) &&
      (left.store.repr.keySet union right.store.repr.keySet).forall { k =>
        left.map(access(k)) <= right.map(access(k))
      }
    }

    override def decompose(state: Dotted[DotMap[K, V]]): Iterable[Dotted[DotMap[K, V]]] = {
      val added = for {
        (k, v)                    <- state.store.repr
        Dotted(atomicV, atomicCC) <- Dotted(v, v.dots).decomposed
      } yield Dotted(DotMap(Map(k -> atomicV)), atomicCC)

      added ++ DottedLattice.decomposedDeletions(state)
    }
  }
}
