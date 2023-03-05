package kofre.dotted

import kofre.base.{Bottom, Lattice}
import kofre.time.{Dot, Dots}

import scala.annotation.targetName

/** Associates from [[Dot]]s to `A`.
  * Can be considered a very bare bones multi value register
  */
case class DotFun[A](repr: Map[Dot, A])

object DotFun {

  def empty[A]: DotFun[A] = DotFun(Map.empty)

  def single[A](dot: Dot, value: A): DotFun[A] = DotFun(Map(dot -> value))

  given dotStore[V]: HasDots[DotFun[V]] with {
    override def getDots(dotStore: DotFun[V]): Dots = Dots.from(dotStore.repr.keys)
  }

  given dottedLattice[A: Lattice]: DottedLattice[DotFun[A]] =
    new DottedLattice[DotFun[A]] {

      /** Partial merging combines the stored values, but ignores the context.
        * Thus enabling nested merging of values, without merging context multiple times.
        */
      override def mergePartial(left: Dotted[DotFun[A]], right: Dotted[DotFun[A]]): DotFun[A] = {
        val fromLeft = left.store.repr.filter { case (dot, _) => !right.context.contains(dot) }
        DotFun(right.store.repr.foldLeft(fromLeft) {
          case (m, (dot, r)) =>
            left.store.repr.get(dot) match {
              case None =>
                if (left.context.contains(dot)) m
                else m.updated(dot, r)
              case Some(l) => m.updated(dot, Lattice[A].merge(l, r))
            }
        })
      }

      override def lteq(left: Dotted[DotFun[A]], right: Dotted[DotFun[A]]): Boolean = {
        if !(left.context <= right.context) then return false

        def `left values are <= than right values` =
          right.store.repr.forall { (k, r) =>
            left.store.repr.get(k).forall { l => l <= r }
          }
        def `right has no values deleted in left` = {
          right.store.dots disjunct left.deletions
        }

        `left values are <= than right values` &&
        `right has no values deleted in left`
      }

      override def decompose(state: Dotted[DotFun[A]]): Iterable[Dotted[DotFun[A]]] = {
        val added =
          Lattice[Map[Dot, A]].decompose(state.store.repr).map { m =>
            val df = DotFun(m)
            Dotted(df, df.dots)
          }

        added ++ DottedDecompose.decomposedDeletions(state)
      }
    }
}
