package kofre.datatypes.alternatives

import kofre.base.{Bottom, Lattice}
import kofre.dotted.{DotFun, Dotted, HasDots}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}

/** An ResettableCounter (Resettable Counter/Add Wins Counter) is a Delta CRDT modeling a counter.
  *
  * Calling fresh after every time that deltas are shipped to other replicas prevents subsequent increment/decrement
  * operations to be overwritten by concurrent reset operations.
  *
  * This counter was originally proposed by Baquera et al.
  * in "The problem with embedded CRDT counters and a solution", see [[https://dl.acm.org/doi/abs/10.1145/2911151.2911159?casa_token=D7n88K9dW7gAAAAA:m3WhHMFZxoCwGFk8DVoqJXBJpwJwrqKMLqtgKo_TSiwU_ErWgOZjo4UqYqDCb-bG3iJlXc_Ti7aB9w here]]
  */
case class ResettableCounter(inner: DotFun[(Int, Int)]) derives Bottom

object ResettableCounter {

  val zero: ResettableCounter = ResettableCounter(DotFun.empty)

  given lattice: Lattice[ResettableCounter] =
    given Lattice[Int] = math.max _
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

    def value(using PermQuery): Int = {
      current.inner.repr.values.foldLeft(0) {
        case (counter, (inc, dec)) => counter + inc - dec
      }
    }

    /** Without using fresh, reset wins over concurrent increments/decrements
      * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
      */
    def fresh(using ReplicaId)(): CausalMutate = {
      val nextDot = context.nextDot(replicaId)

      deltaState(
        df = Some(ResettableCounter(DotFun(Map(nextDot -> ((0, 0)))))),
        cc = Dots.single(nextDot)
      ).mutator
    }

    private def update(using ReplicaId, PermCausalMutate)(u: (Int, Int)): C = {
      context.max(replicaId) match {
        case Some(currentDot) if current.inner.repr.contains(currentDot) =>
          val newCounter = (current.inner.repr(currentDot), u) match {
            case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
          }

          deltaState(
            df = Some(ResettableCounter(DotFun(current.inner.repr + (currentDot -> newCounter)))),
            cc = Dots.single(currentDot)
          ).mutator
        case _ =>
          val nextDot = context.nextDot(replicaId)

          deltaState(
            df = Some(ResettableCounter(DotFun(Map((nextDot -> u))))),
            cc = Dots.single(nextDot)
          ).mutator
      }
    }

    def increment(using ReplicaId, PermCausalMutate)(): C = update((1, 0))

    def decrement(using ReplicaId, PermCausalMutate)(): C = update((0, 1))

    def reset()(using PermCausalMutate): C = {
      deltaState(
        cc = Dots.from(current.inner.repr.keySet)
      ).mutator
    }
  }
}
