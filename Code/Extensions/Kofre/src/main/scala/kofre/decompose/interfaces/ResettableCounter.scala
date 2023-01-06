package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice}
import kofre.time.Dots
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dot
import kofre.dotted.{DotFun, Dotted, DottedDecompose}

/** An ResettableCounter (Resettable Counter/Add Wins Counter) is a Delta CRDT modeling a counter.
  *
  * Calling fresh after every time that deltas are shipped to other replicas prevents subsequent increment/decrement
  * operations to be overwritten by concurrent reset operations.
  *
  * This counter was originally proposed by Baquera et al.
  * in "The problem with embedded CRDT counters and a solution", see [[https://dl.acm.org/doi/abs/10.1145/2911151.2911159?casa_token=D7n88K9dW7gAAAAA:m3WhHMFZxoCwGFk8DVoqJXBJpwJwrqKMLqtgKo_TSiwU_ErWgOZjo4UqYqDCb-bG3iJlXc_Ti7aB9w here]]
  */
case class ResettableCounter(inner: DotFun[(Int, Int)]) derives Bottom, DottedDecompose

object ResettableCounter {

  val zero: ResettableCounter = ResettableCounter(DotFun.empty)

  private def deltaState(
      df: Option[ResettableCounter] = None,
      cc: Dots
  ): Dotted[ResettableCounter] = {
    Dotted(
      df.getOrElse(zero),
      cc
    )
  }

  implicit class RCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, ResettableCounter](container) {

    def value(using QueryP): Int = {
      current.inner.store.values.foldLeft(0) {
        case (counter, (inc, dec)) => counter + inc - dec
      }
    }

    /** Without using fresh, reset wins over concurrent increments/decrements
      * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
      */
    def fresh()(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)

      deltaState(
        df = Some(ResettableCounter(DotFun(Map(nextDot -> ((0, 0)))))),
        cc = Dots.single(nextDot)
      ).mutator
    }

    private def update(u: (Int, Int))(using CausalMutationP, IdentifierP): C = {
      context.max(replicaID) match {
        case Some(currentDot) if current.inner.store.contains(currentDot) =>
          val newCounter = (current.inner(currentDot), u) match {
            case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
          }

          deltaState(
            df = Some(ResettableCounter(current.inner + (currentDot -> newCounter))),
            cc = Dots.single(currentDot)
          ).mutator
        case _ =>
          val nextDot = context.nextDot(replicaID)

          deltaState(
            df = Some(ResettableCounter(DotFun(Map((nextDot -> u))))),
            cc = Dots.single(nextDot)
          ).mutator
      }
    }

    def increment()(using CausalMutationP, IdentifierP): C = update((1, 0))

    def decrement()(using CausalMutationP, IdentifierP): C = update((0, 1))

    def reset()(using CausalMutationP): C = {
      deltaState(
        cc = Dots.from(current.inner.keySet)
      ).mutator
    }
  }
}
