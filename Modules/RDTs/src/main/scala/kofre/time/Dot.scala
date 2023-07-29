package kofre.time

import kofre.base.{Uid, Time}

/** A Dot is a globally unique point in time.
  * Dots are partially ordered by their time per replicaId.
  */
case class Dot(replicaId: Uid, time: Time) {
  def advance: Dot = Dot(replicaId, time + 1)
}

object Dot {
  /** While this seems plausible, it might, in general, be better to treat all dots as incomparable, we assume them to increase monotonically, but that is for optimization purposes, not because we use it anywhere else */
  @deprecated("probably not a good idea")
  def partialOrdering: PartialOrdering[Dot] = new:
    override def tryCompare(x: Dot, y: Dot): Option[Int] =
      if x.replicaId == y.replicaId
      then Some(Ordering[Long].compare(x.time, y.time))
      else None
    override def lteq(x: Dot, y: Dot): Boolean =
      x.replicaId == y.replicaId && x.time <= y.time
}

class CausalityException(msg: String) extends Exception(msg)
