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
  def apply[A](using wcm: WithContextMerge[A]) = wcm

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
}
