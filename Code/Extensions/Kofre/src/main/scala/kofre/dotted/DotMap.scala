package kofre.dotted

import kofre.base.Bottom
import kofre.time.Dots
import kofre.contextual.ContextDecompose.FromConlattice
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, WithContext}
import kofre.decompose.interfaces

case class DotMap[K, V](repr: Map[K, V]) {
  def dots(using ccv: AsCausalContext[V]): Dots =
    repr.valuesIterator.foldLeft(Dots.empty)((acc, v) => acc.union(ccv.dots(v)))
  export repr.{repr as _, *}
}

/** DotMap is a dot store implementation that maps keys of an arbitrary type K to values of a dot store type V. See
  * [[interfaces.ORMapInterface]] for a usage example.
  */
object DotMap {

  def empty[K, V]: DotMap[K, V] = DotMap(Map.empty)

  given asCausalContext[K, V: AsCausalContext]: AsCausalContext[DotMap[K, V]] with {
    override def dots(a: DotMap[K, V]): Dots = a.dots

  }

  /** This essentially lifts the [[ContextLattice]] of [[V]] to a [[ DotMap[K, V ] ]].
    * Recursively merging values present in both maps with the given context.
    */
  given contextLattice[K, V: ContextLattice: Bottom]: ContextLattice[DotMap[K, V]] with {
    override def mergePartial(left: WithContext[DotMap[K, V]], right: WithContext[DotMap[K, V]]): DotMap[K, V] = {
      DotMap((left.store.repr.keySet union right.store.repr.keySet).flatMap { key =>
        val leftCausalStore  = left.map(_.getOrElse(key, Bottom.empty[V]))
        val rightCausalStore = right.map(_.getOrElse(key, Bottom.empty[V]))
        val res              = leftCausalStore conmerge rightCausalStore
        if Bottom.empty[V] == res then None else Some(key -> res)
      }.toMap)
    }
  }

  given contextDecompose[K, V: ContextDecompose: AsCausalContext]: ContextDecompose[DotMap[K, V]] =
    new FromConlattice[DotMap[K, V]](contextLattice) {

      override def empty: WithContext[DotMap[K, V]] = WithContext(DotMap.empty)

      override def lteq(left: WithContext[DotMap[K, V]], right: WithContext[DotMap[K, V]]): Boolean = {
        def firstCondition = (left.context subtract right.context).isEmpty

        def secondConditionHelper(keys: Iterable[K]): Boolean = keys.forall { k =>
          left.map(_.getOrElse(k, Bottom.empty[V])) <= right.map(_.getOrElse(k, Bottom.empty[V]))
        }

        def secondCondition = secondConditionHelper(left.store.keys) && secondConditionHelper(right.store.keys)

        firstCondition && secondCondition
      }

      override def decompose(state: WithContext[DotMap[K, V]]): Iterable[WithContext[DotMap[K, V]]] = {
        val added = for {
          k <- state.store.keys
          WithContext(atomicV, atomicCC) <- {
            val v = state.store.getOrElse(k, Bottom.empty[V])
            ContextDecompose[V].decompose(WithContext(v, AsCausalContext[V].dots(v)))
          }
        } yield WithContext(DotMap(Map(k -> atomicV)), atomicCC)

        val removed =
          state.context.subtract(state.store.dots).decomposed.map(WithContext(
            DotMap.empty[K, V],
            _
          ))

        added ++ removed
      }
    }
}
