package kofre.contextual

import kofre.base.{DecomposeLattice, Lattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.AsCausalContext
import kofre.base.Lattice.Operators
import kofre.base.Bottom

import scala.annotation.targetName

/** The delta CRDT paper introduces a lot of individual abstractions
  * that all do a lot of cool stuff, but often not separated into their pieces.
  * This is one of those pieces systematically handling removals based on metadata.
  *
  * The context encodes dots that have been seen.
  * The store of type [[A]] is assumed to be something that stores individual information per dot.
  * We represent removals as context that contains a dot, but a store that no longer contains the dot.
  * Thus, when merging the abstract logic is somewhat along the lines of:
  *   Both contain the (dot, value) pair? => merge value recursively.
  *   Only one contains the dot in the context? => keep that value.
  *   Both contain the dot, but at least one has no matching value? => remove all values for that dot.
  *
  * Separating into a [[mergePartial]] allows extracting the context into the outermost layer reducing metadata overhead.
  */
trait WithContextMerge[A] extends Lattice[WithContext[A]] {
  def mergePartial(left: WithContext[A], right: WithContext[A]): A

  def merge(left: WithContext[A], right: WithContext[A]): WithContext[A] =
    WithContext(
      mergePartial(left, right),
      left.context.merge(right.context)
    )

  extension (left: WithContext[A]) def conmerge(right: WithContext[A]): A = mergePartial(left, right)
}

object WithContextMerge {
  def apply[A](using wcm: WithContextMerge[A]): WithContextMerge[A] = wcm

  /** This essentially tracks the currently present dots, and all dots */
  given causalContext: WithContextMerge[CausalContext] with {
    override def mergePartial(left: WithContext[CausalContext], right: WithContext[CausalContext]): CausalContext = {
      val fromLeft  = left.store subtract right.context
      val fromRight = right.store.subtract(left.context subtract left.store)

      fromLeft union fromRight
    }
  }

  /** The context describes dots that have been seen.
    * The store describes which value is associated for a given dot.
    * Dots that are removed from the store are considered deleted.
    * All others are merged as for normal maps.
    *
    * The delta CRDT paper calls this a DotFun
    */
  given perDot[A: Lattice]: WithContextMerge[Map[Dot, A]] = (left, right) => {
    val fromLeft = left.store.filter { case (dot, _) => !right.context.contains(dot) }

    right.store.foldLeft(fromLeft) {
      case (m, (dot, r)) =>
        left.store.get(dot) match {
          case None =>
            if (left.context.contains(dot)) m
            else m.updated(dot, r)
          case Some(l) => m.updated(dot, Lattice[A].merge(l, r))
        }
    }
  }

  /** This essentially lifts the [[WithContextMerge]] of [[V]] to a [[ Map[K, V] ] ]].
    * Recursively merging values present in both maps with the given context.
    */
  given dotMapMerge[K, V: WithContextMerge: Bottom]: WithContextMerge[Map[K, V]] with {
    override def mergePartial(left: WithContext[Map[K, V]], right: WithContext[Map[K, V]]): Map[K, V] = {
      (left.store.keySet union right.store.keySet).flatMap { key =>
        val leftCausalStore  = WithContext(left.store.getOrElse(key, Bottom.empty[V]), left.context)
        val rightCausalStore = WithContext(right.store.getOrElse(key, Bottom.empty[V]), right.context)
        val res              = WithContextMerge[V].mergePartial(leftCausalStore, rightCausalStore)
        if Bottom.empty[V] == res then None else Some(key -> res)
      }.toMap
    }
  }

  given pairPartialMerge[A: WithContextMerge, B: WithContextMerge]: WithContextMerge[(A, B)] with {
    override def mergePartial(left: WithContext[(A, B)], right: WithContext[(A, B)]): (A, B) =
      val WithContext((left1, left2), leftCContext)    = left
      val WithContext((right1, right2), rightCContext) = right
      val stateMerged1 = WithContext(left1, leftCContext) conmerge WithContext(right1, rightCContext)
      val stateMerged2 = WithContext(left2, leftCContext) conmerge WithContext(right2, rightCContext)
      (stateMerged1, stateMerged2)
  }
}
