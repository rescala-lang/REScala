package kofre.time

import kofre.base.{Lattice, Orderings}

import java.util.concurrent.atomic.AtomicLong
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random

/** This is a causally consistent wall-time clock that provides a total order in a distributed environment.
  * It tries to be just wall-time clock if possible – this is the case when no updates happen “concurrently” where the concurrency window depends on the accuracy of different clocks (this is on the order of milliseconds even with well synchronized clocks).
  * In case where there is clock drift, it may happen that the current wall-time clock is behind the observed `time`. Then we fall back to increasing the `causal` counter. This works, but has a weird “whoever does the most updates is the newest” semantics, which is undesirable in many practical use-cases.
  * The `random` parameter is used for enforcing totality in cases where the `causal` values also happen to collide. It is slightly preferrable if `random` is monotonically increasing on a single replica, as then updates within the same millisecond from the same replica do not need to increment the causal counter.
  */
case class CausalTime(time: Time, causal: Long, random: Long):
  def causalIncrement: CausalTime = CausalTime(time, causal + 1, CausalTime.countedTime())
  def advance: CausalTime =
    val now = CausalTime.now()
    if now <= this
    then causalIncrement
    else now

object CausalTime:
  given ordering: Ordering[CausalTime] = Orderings.lexicographic

  given lattice: Lattice[CausalTime] = Lattice.fromOrdering(using ordering)

  // originally used `System.nanoTime` for the third component, but the Web does not offer high precision timers, so a counter it is!
  private val timeCounter = AtomicLong(Random.nextLong())
  def countedTime()       = timeCounter.getAndIncrement()
  def now()               = CausalTime(Time.current(), 0, countedTime())
