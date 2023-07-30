package kofre.time

import scala.math.Ordering.Implicits.infixOrderingOps

case class CausalTime(time: Time, causal: Long, random: Long):
  def inc: CausalTime = CausalTime(time, causal + 1, System.nanoTime())
  def advance: CausalTime =
    val now = CausalTime.now()
    if now <= this
    then inc
    else now

object CausalTime:
  given ordering: Ordering[CausalTime] =
    Ordering.by[CausalTime, Long](_.time)
      .orElseBy(_.causal)
      .orElseBy(_.random)
  def now() = CausalTime(Time.current(), 0, System.nanoTime())
