package rdts.datatypes.alternatives

import rdts.base.{Bottom, Lattice}
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.{OpsSyntaxHelper, LocalReplicaId}
import rdts.time.{Dot, Dots}

/** An ResettableCounter (Resettable Counter/Add Wins Counter) is a Delta CRDT modeling a counter.
  *
  * Calling fresh after every time that deltas are shipped to other replicas prevents subsequent increment/decrement
  * operations to be overwritten by concurrent reset operations.
  *
  * This counter was originally proposed by Baquera et al.
  * in "The problem with embedded CRDT counters and a solution", see [[https://dl.acm.org/doi/abs/10.1145/2911151.2911159?casa_token=D7n88K9dW7gAAAAA:m3WhHMFZxoCwGFk8DVoqJXBJpwJwrqKMLqtgKo_TSiwU_ErWgOZjo4UqYqDCb-bG3iJlXc_Ti7aB9w here]]
  */
case class ResettableCounter(inner: Map[Dot, (Int, Int)]) derives Bottom

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

  extension [C](container: C)
    def resettableCounter: syntax[C] = syntax(container)

  implicit class syntax[C](container: C) extends OpsSyntaxHelper[C, ResettableCounter](container) {

    def value(using IsQuery): Int = {
      current.inner.values.foldLeft(0) {
        case (counter, (inc, dec)) => counter + inc - dec
      }
    }

    /** Without using fresh, reset wins over concurrent increments/decrements
      * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
      */
    def fresh(using LocalReplicaId)(): CausalMutator = {
      val nextDot = context.nextDot(replicaId)

      deltaState(
        df = Some(ResettableCounter(Map(nextDot -> ((0, 0))))),
        cc = Dots.single(nextDot)
      ).mutator
    }

    private def update(using LocalReplicaId, IsCausalMutator)(u: (Int, Int)): C = {
      context.max(replicaId) match {
        case Some(currentDot) if current.inner.contains(currentDot) =>
          val newCounter = (current.inner(currentDot), u) match {
            case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
          }

          deltaState(
            df = Some(ResettableCounter(Map(currentDot -> newCounter))),
            cc = Dots.single(currentDot)
          ).mutator
        case _ =>
          val nextDot = context.nextDot(replicaId)

          deltaState(
            df = Some(ResettableCounter(Map((nextDot -> u)))),
            cc = Dots.single(nextDot)
          ).mutator
      }
    }

    def increment(using LocalReplicaId, IsCausalMutator)(): C = update((1, 0))

    def decrement(using LocalReplicaId, IsCausalMutator)(): C = update((0, 1))

    def reset()(using IsCausalMutator): C = {
      deltaState(
        cc = Dots.from(current.inner.keySet)
      ).mutator
    }
  }
}
