package rdts.datatypes.alternatives

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.alternatives.ResettableCounter.deltaState
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}

/** An ResettableCounter (Resettable Counter/Add Wins Counter) is a Delta CRDT modeling a counter.
  *
  * Calling fresh after every time that deltas are shipped to other replicas prevents subsequent increment/decrement
  * operations to be overwritten by concurrent reset operations.
  *
  * This counter was originally proposed by Baquera et al.
  * in "The problem with embedded CRDT counters and a solution", see [[https://dl.acm.org/doi/abs/10.1145/2911151.2911159?casa_token=D7n88K9dW7gAAAAA:m3WhHMFZxoCwGFk8DVoqJXBJpwJwrqKMLqtgKo_TSiwU_ErWgOZjo4UqYqDCb-bG3iJlXc_Ti7aB9w here]]
  */
case class ResettableCounter(inner: Map[Dot, (Int, Int)]) derives Bottom {

  type Delta = Dotted[ResettableCounter]

  def value: Int = {
    inner.values.foldLeft(0) {
      case (counter, (inc, dec)) => counter + inc - dec
    }
  }

  /** Without using fresh, reset wins over concurrent increments/decrements
    * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
    */
  def fresh(using LocalUid)()(using context: Dots): Delta = {
    val nextDot = context.nextDot(LocalUid.replicaId)

    deltaState(
      df = Some(ResettableCounter(Map(nextDot -> ((0, 0))))),
      cc = Dots.single(nextDot)
    )
  }

  private def update(using LocalUid)(u: (Int, Int))(using context: Dots): Delta = {
    context.max(LocalUid.replicaId) match {
      case Some(currentDot) if inner.contains(currentDot) =>
        val newCounter = (inner(currentDot), u) match {
          case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
        }

        deltaState(
          df = Some(ResettableCounter(Map(currentDot -> newCounter))),
          cc = Dots.single(currentDot)
        )
      case _ =>
        val nextDot = context.nextDot(LocalUid.replicaId)

        deltaState(
          df = Some(ResettableCounter(Map((nextDot -> u)))),
          cc = Dots.single(nextDot)
        )
    }
  }

  def increment(using LocalUid)()(using context: Dots): Delta = update((1, 0))

  def decrement(using LocalUid)()(using context: Dots): Delta = update((0, 1))

  def reset(): Delta = {
    deltaState(
      cc = Dots.from(inner.keySet)
    )
  }
}

object ResettableCounter {

  val zero: ResettableCounter = ResettableCounter(Map.empty)

  given lattice: Lattice[ResettableCounter] =
    given Lattice[Int] = math.max
    Lattice.derived

  given hasDots: HasDots[ResettableCounter] = HasDots.derived

  private def deltaState(
      df: Option[ResettableCounter] = None,
      cc: Dots
  ): Dotted[ResettableCounter] = {
    Dotted(
      df.getOrElse(zero),
      cc
    )
  }

}
