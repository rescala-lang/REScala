package kofre.decompose.interfaces

import kofre.causality.CausalContext
import kofre.decompose.*
import kofre.syntax.{DeltaMutator, OpsSyntaxHelper}
import kofre.decompose.DotStore.*
import kofre.decompose.interfaces.PNCounterModule.PNCounter
import kofre.dotbased.CausalStore


/** An RCounter (Resettable Counter/Add Wins Counter) is a Delta CRDT modeling a counter.
  *
  * Calling fresh after every time that deltas are shipped to other replicas prevents subsequent increment/decrement
  * operations to be overwritten by concurrent reset operations.
  *
  * This counter was originally proposed by Baquera et al.
  * in "The problem with embedded CRDT counters and a solution", see [[https://dl.acm.org/doi/abs/10.1145/2911151.2911159?casa_token=D7n88K9dW7gAAAAA:m3WhHMFZxoCwGFk8DVoqJXBJpwJwrqKMLqtgKo_TSiwU_ErWgOZjo4UqYqDCb-bG3iJlXc_Ti7aB9w here]]
  */
object RCounterInterface {
  implicit def IntPairAsUIJDLattice: UIJDLattice[(Int, Int)] = new UIJDLattice[(Int, Int)] {
    override def leq(left: (Int, Int), right: (Int, Int)): Boolean = (left, right) match {
      case ((linc, ldec), (rinc, rdec)) =>
        linc <= rinc && ldec <= rdec
    }

    /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: (Int, Int)): Iterable[(Int, Int)] = state match {
      case (inc, dec) => List((inc, 0), (0, dec))
    }

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: (Int, Int), right: (Int, Int)): (Int, Int) = (left, right) match {
      case ((linc, ldec), (rinc, rdec)) =>
        (linc max rinc, ldec max rdec)
    }

    override def bottom: (Int, Int) = (0, 0)
  }

  type RCounter = CausalStore[DotFun[(Int, Int)]]

  private def deltaState(
      df: Option[DotFun[(Int, Int)]] = None,
      cc: C
  ): RCounter = {
    val bottom = UIJDLattice[RCounter].bottom

    CausalStore(
      df.getOrElse(bottom.store),
      cc
    )
  }

  implicit class RCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, RCounter](container) {

    def value(using QueryP): Int = {
      current.store.values.foldLeft(0) {
        case (counter, (inc, dec)) => counter + inc - dec
      }
    }

    /** Without using fresh, reset wins over concurrent increments/decrements
      * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
      */
    def fresh()(using MutationIDP): C = {
      val nextDot = current.context.nextDot(replicaID)

      deltaState(
        df = Some(DotFun[(Int, Int)].empty + (nextDot -> ((0, 0)))),
        cc = CausalContext.one(nextDot)
      )
    }

    private def update(u: (Int, Int))(using MutationIDP): C = {
      current.context.max(replicaID) match {
        case Some(currentDot) if current.store.contains(currentDot) =>
          val newCounter = (current.store(currentDot), u) match {
            case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
          }

          deltaState(
            df = Some(current.store + (currentDot -> newCounter)),
            cc = CausalContext.one(currentDot)
          )
        case _ =>
          val nextDot = current.context.nextDot(replicaID)

          deltaState(
            df = Some(DotFun[(Int, Int)].empty + (nextDot -> u)),
            cc = CausalContext.one(nextDot)
          )
      }
    }

    def increment()(using MutationIDP): C = update((1, 0))

    def decrement()(using MutationIDP): C = update((0, 1))

    def reset()(using MutationIDP): C = {
      deltaState(
        cc = CausalContext.fromSet(current.store.keySet)
      )
    }
  }
}
