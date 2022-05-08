package kofre.decompose.interfaces

import kofre.causality.CausalContext
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.contextual.WithContextDecompose.*
import kofre.contextual.WithContext
import kofre.predef.Epoche

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
object EnableWinsFlag {
  type EWFlag = WithContext[EWFlagPlain]
  type EWFlagPlain = CausalContext

  /** Its enabled if there is a value in the store.
    * It relies on the external context to track removals.
    */
  implicit class EnableWinsFlagOps[C](container: C) extends OpsSyntaxHelper[C, EWFlagPlain](container) {
    def read(using QueryP): Boolean = !current.isEmpty

    def enable()(using IdentifierP, QueryP, CausalMutation, CausalP): C = {
      val nextDot = context.nextDot(replicaID)
      WithContext(
        CausalContext.single(nextDot),
        current add nextDot
      )
    }
    def disable()(using QueryP, CausalMutation): C = {
      WithContext(
        CausalContext.empty,
        current
      )
    }
  }

}
