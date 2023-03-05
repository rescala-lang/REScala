package kofre.dotted

import kofre.base.{Bottom, Lattice}
import kofre.time.{Dot, Dots}

import scala.annotation.targetName

/** Associates from [[Dot]]s to `A`. */
case class DotFun[A](repr: Map[Dot, A])

object DotFun {

  def empty[A]: DotFun[A] = DotFun(Map.empty)

  def single[A](dot: Dot, value: A): DotFun[A] = DotFun(Map(dot -> value))

  given dotStore[V]: HasDots[DotFun[V]] with {
    override def getDots(dotStore: DotFun[V]): Dots = Dots.from(dotStore.repr.keys)
  }

  given dottedLattice[A: Lattice]: DottedLattice[DotFun[A]] =
    new DottedLattice[DotFun[A]] {

      override def mergePartial(left: Dotted[DotFun[A]], right: Dotted[DotFun[A]]): DotFun[A] = {
        val fromLeft = left.store.repr.filter { case (dot, _) => !right.knows(dot) }
        val fromCombined =
          right.store.repr.iterator.flatMap {
            case (dot, r) =>
              left.store.repr.get(dot) match {
                case None =>
                  // was it deleted in left, or not yet inserted?
                  if left.knows(dot)
                  then None
                  else Some(dot -> r)
                case Some(l) => Some(dot -> Lattice[A].merge(l, r))
              }
          }
        DotFun(fromLeft ++ fromCombined)
      }

      /** Insertion is larger. Removals are larger. Otherwise compare the value for each dot. */
      override def lteq(left: Dotted[DotFun[A]], right: Dotted[DotFun[A]]): Boolean = {
        // invariant on lteq
        (left.context <= right.context) &&
        // deletions are larger
        (right.store.dots disjunct left.deletions) &&
        // everything not in the right store will be deleted from left on merge
        // things that are still in left must be smaller
        // things that are not in left are not there yet (otherwise the deletion clause above would hold)
        right.store.repr.forall { (k, r) =>
          left.store.repr.get(k).forall { l => l <= r }
        }
      }

      override def decompose(state: Dotted[DotFun[A]]): Iterable[Dotted[DotFun[A]]] = {
        val added = Lattice[Map[Dot, A]].decompose(state.store.repr).map { m =>
          val df = DotFun(m)
          Dotted(df, df.dots)
        }
        added ++ DottedDecompose.decomposedDeletions(state)
      }
    }
}
