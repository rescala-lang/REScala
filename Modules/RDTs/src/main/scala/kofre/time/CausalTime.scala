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

  // originally used `System.nanoTime` for the third component, but the Web does not offer high precision timers, so a counter it is!
  @volatile private var timeCounter = 0L
  def now() = CausalTime(Time.current(), 0, {timeCounter += 1; timeCounter})
