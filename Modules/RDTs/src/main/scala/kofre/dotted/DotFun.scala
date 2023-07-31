package kofre.dotted

import kofre.base.Lattice
import kofre.time.{Dot, Dots}

/** A dot fun tracks a set of values associated to a certain point in time.
  * This makes them useful as both [[kofre.datatypes.contextual.MultiVersionRegister]] and simple observe remove sets/maps.
  */
case class DotFun[A](repr: Map[Dot, A])

object DotFun {

  def empty[A]: DotFun[A] = DotFun(Map.empty)

  def single[A](dot: Dot, value: A): DotFun[A] = DotFun(Map(dot -> value))

  given hasDots[V]: HasDots[DotFun[V]] with {
    extension (value: DotFun[V])
      override def dots: Dots = Dots.from(value.repr.keys)

      override def removeDots(dots: Dots): Option[DotFun[V]] =
        val res = value.repr.filter((dot, _) => !dots.contains(dot))
        if res.isEmpty then None
        else Some(DotFun(res))
  }

  given lattice[A: Lattice]: Lattice[DotFun[A]] = Lattice.derived
}
