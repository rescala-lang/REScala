package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice}
import kofre.time.Dots
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.contextual.ContextDecompose.*
import kofre.contextual.{AsCausalContext, ContextDecompose, WithContext}
import kofre.dotted.DotSet
import kofre.datatypes.Epoche

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
case class EnableWinsFlag(inner: DotSet)

object EnableWinsFlag {

  given contextDecompose: ContextDecompose[EnableWinsFlag] = ContextDecompose.derived
  given asCausalContextEWF: AsCausalContext[EnableWinsFlag] with {
    override def dots(a: EnableWinsFlag): Dots = a.inner.repr
  }

  val empty: EnableWinsFlag = EnableWinsFlag(DotSet.empty)

  /** It is enabled if there is a value in the store.
    * It relies on the external context to track removals.
    */
  implicit class EnableWinsFlagOps[C](container: C) extends OpsSyntaxHelper[C, EnableWinsFlag](container) {
    def read(using QueryP): Boolean = !current.inner.repr.isEmpty

    def enable()(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)
      WithContext(
        EnableWinsFlag(DotSet(Dots.single(nextDot))),
        current.inner.repr add nextDot
        ).mutator
    }
    def disable()(using CausalMutationP): C = {
      WithContext(
        EnableWinsFlag(DotSet(Dots.empty)),
        current.inner.repr
        ).mutator
    }
  }

}
