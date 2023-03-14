package kofre.dotted

import kofre.base.Lattice
import kofre.dotted.DottedLattice.Partitioned
import kofre.time.{Dot, Dots}

import scala.annotation.targetName

/** A dot fun tracks a set of values associated to a certain point in time.
  * This makes them useful as both [[kofre.datatypes.MultiVersionRegister]] and simple observe remove sets/maps.
  */
case class DotFun[A](repr: Map[Dot, A])

object DotFun {

  def empty[A]: DotFun[A] = DotFun(Map.empty)

  def single[A](dot: Dot, value: A): DotFun[A] = DotFun(Map(dot -> value))

  given dotStore[V]: HasDots[DotFun[V]] with {
    extension (value: DotFun[V])
      override def dots: Dots = Dots.from(value.repr.keys)

      override def removeDots(dots: Dots): Option[DotFun[V]] =
        val res = value.repr.filter((dot, v) => !dots.contains(dot))
        if res.isEmpty then None
        else Some(DotFun(res))
  }

  given dottedLattice[A: Lattice]: DottedLattice[DotFun[A]] with {

    override def mergePartial(left: Dotted[DotFun[A]], right: Dotted[DotFun[A]]): DotFun[A] = {
      val fromLeft = left.data.repr.filter { case (dot, _) => !right.knows(dot) }
      val fromCombined =
        right.data.repr.iterator.flatMap {
          case (dot, r) =>
            left.data.repr.get(dot) match {
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
      (right.contained disjunct left.deletions) &&
      // everything not in the right store will be deleted from left on merge
      // things that are still in left must be smaller
      // things that are not in left are not there yet (otherwise the deletion clause above would hold)
      right.data.repr.forall { (k, r) =>
        left.data.repr.get(k).forall { l => l <= r }
      }
    }

    override def decompose(state: Dotted[DotFun[A]]): Iterable[Dotted[DotFun[A]]] = {
      val added = Lattice[Map[Dot, A]].decompose(state.data.repr).map { m =>
        val df = DotFun(m)
        Dotted(df, df.dots)
      }
      added ++ DottedLattice.decomposedDeletions(state)
    }
  }
}
