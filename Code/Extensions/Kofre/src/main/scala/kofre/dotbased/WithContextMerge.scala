package kofre.dotbased

import kofre.Lattice
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.AsCausalContext
import kofre.Lattice.Operators
import kofre.decompose.{DecomposableDotStore, UIJDLattice}

trait WithContextMerge[A] {
  def mergePartial(left: WithContext[A], right: WithContext[A]): A
}
object WithContextMerge {
  def apply[A](using wcm: WithContextMerge[A]): WithContextMerge[A] = wcm

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

  given dotMapMerge[K, V: WithContextMerge: AsCausalContext]: WithContextMerge[Map[K, V]] with {
    def mergePartial(
        left: WithContext[Map[K, V]],
        right: WithContext[Map[K, V]]
    ): Map[K, V] = {
      def mergeHelp(l: V, r: V): Option[V] = {
        val mergedVal =
          WithContextMerge[V].mergePartial(WithContext(l, left.context), WithContext(r, right.context))
        if (mergedVal == AsCausalContext[V].empty) None
        else Some(mergedVal)
      }

      var rightSet = right.context

      val added = right.store.foldLeft(left.store) { case (currentLeft, (k, r)) =>
        rightSet = rightSet.subtract(r.asContext)
        currentLeft.updatedWith(k) {
          case None    => mergeHelp(AsCausalContext[V].empty, r)
          case Some(l) => mergeHelp(l, r)
        }
      }

      if (rightSet.isEmpty) added
      else {
        added.foldLeft(added) { case (current, (k, l)) =>
          if (right.store.contains(k)) current
          else mergeHelp(l, AsCausalContext[V].empty) match {
            case None         => current.removed(k)
            case Some(merged) => current.updated(k, merged)
          }
        }
      }
    }
  }
}
