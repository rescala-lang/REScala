package kofre.decompose.interfaces

import kofre.causality.CausalContext
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.decompose.DotStore.*
import kofre.dotbased.CausalStore
import kofre.primitives.Epoche

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
object EWFlagInterface {
  type EWFlag = CausalStore[CausalContext]

  implicit class EWFlagSyntax[C](container: C) extends OpsSyntaxHelper[C, EWFlag](container) {
    def read(using QueryP): Boolean = !current.store.isEmpty

    def enable()(using MutationIDP): C = {
      val nextDot = current.context.nextDot(replicaID)
      CausalStore(
        CausalContext.single(nextDot),
        current.store add nextDot
      )
    }
    def disable()(using MutationP): C = {
      CausalStore(
        CausalContext.empty,
        current.store
      )
    }
  }

}
