package kofre.dotted

import kofre.base.{Bottom, Lattice}
import kofre.time.{Dot, Dots}

import scala.annotation.targetName

/** Associates from [[Dot]]s to `A`.
  * Can be considered a very bare bones multi value register */
case class DotFun[A](store: Map[Dot, A])

object DotFun {

  def empty[A]: DotFun[A] = DotFun(Map.empty)

  def single[A](dot: Dot, value: A): DotFun[A] = DotFun(Map(dot -> value))

  given dotStore[V]: HasDots[DotFun[V]] with {
    override def getDots(dotStore: DotFun[V]): Dots =  Dots.from(dotStore.store.keysIterator)
  }

  given dottedLattice[A: Lattice]: DottedLattice[DotFun[A]] =
    new DottedLattice[DotFun[A]] {

      /** Partial merging combines the stored values, but ignores the context.
        * Thus enabling nested merging of values, without merging context multiple times.
        */
      override def mergePartial(left: Dotted[DotFun[A]], right: Dotted[DotFun[A]]): DotFun[A] = {
        val fromLeft = left.store.store.filter { case (dot, _) => !right.context.contains(dot) }
        DotFun(right.store.store.foldLeft(fromLeft) {
          case (m, (dot, r)) =>
            left.store.store.get(dot) match {
              case None =>
                if (left.context.contains(dot)) m
                else m.updated(dot, r)
              case Some(l) => m.updated(dot, Lattice[A].merge(l, r))
            }
        })
      }

      override def lteq(left: Dotted[DotFun[A]], right: Dotted[DotFun[A]]): Boolean = {
        def rightKnowLeft = left.context.forall(right.context.contains)
        def secondCondition = right.store.store.keySet.forall { k =>
          left.store.store.get(k).forall { l => Lattice[A].lteq(l, right.store.store(k)) }
        }
        def thirdCondition = {
          val diff = left.context.diff(left.store.dots)
          right.store.dots.intersect(diff).isEmpty
        }

        rightKnowLeft && secondCondition && thirdCondition
      }

      override def decompose(state: Dotted[DotFun[A]]): Iterable[Dotted[DotFun[A]]] = {
        val added: Iterator[Dotted[DotFun[A]]] = for {
          d <- state.store.dots.iterator
          v <- Lattice[A].decompose(state.store.store(d))
        } yield Dotted(DotFun(Map(d -> v)), Dots.single(d))

        val removed =
          state.context.subtract(state.store.dots).decomposed.map(Dotted(
            DotFun.empty[A],
            _
          ))

        removed ++ added
      }
    }
}
