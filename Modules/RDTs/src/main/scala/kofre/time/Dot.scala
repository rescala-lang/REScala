package kofre.time

import kofre.base.Uid
import kofre.dotted.HasDots

/** A Dot is a globally unique point in time.
  * Dots are partially ordered by their time per replicaId.
  */
case class Dot(place: Uid, time: Time) {
  def advance: Dot = Dot(place, time + 1)
}

object Dot {

  /** While this seems plausible, it might, in general, be better to treat all dots as incomparable, we assume them to increase monotonically, but that is for optimization purposes, not because we use it anywhere else */
  @deprecated("probably not a good idea")
  def partialOrdering: PartialOrdering[Dot] = new:
    override def tryCompare(x: Dot, y: Dot): Option[Int] =
      if x.place == y.place
      then Some(Ordering[Long].compare(x.time, y.time))
      else None
    override def lteq(x: Dot, y: Dot): Boolean =
      x.place == y.place && x.time <= y.time

  given hasDot: HasDots[Dot] with
    extension (dotted: Dot)
      def dots: Dots = Dots.single(dotted)
      def removeDots(dots: Dots): Option[Dot] =
        if dots.contains(dotted) then None else Some(dotted)
}
