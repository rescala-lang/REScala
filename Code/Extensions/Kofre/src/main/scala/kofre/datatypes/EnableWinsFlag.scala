package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.Epoche
import kofre.dotted.{DotSet, Dotted, DottedDecompose, HasDots}
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dots

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
case class EnableWinsFlag(inner: DotSet) derives Bottom

object EnableWinsFlag {

  given contextDecompose: DottedDecompose[EnableWinsFlag] = DottedDecompose.derived
  given hasDotsEWF: HasDots[EnableWinsFlag] with {
    override def dots(a: EnableWinsFlag): Dots = a.inner.repr
  }

  val empty: EnableWinsFlag = EnableWinsFlag(DotSet.empty)

  extension [C](container: C)
    def enableWinsFlag: syntax[C] = syntax(container)

  /** It is enabled if there is a value in the store.
    * It relies on the external context to track removals.
    */
  implicit class syntax[C](container: C) extends OpsSyntaxHelper[C, EnableWinsFlag](container) {
    def read(using PermQuery): Boolean = !current.inner.repr.isEmpty

    def enable(using PermCausalMutate, PermId)(): C = {
      val nextDot = context.nextDot(replicaID)
      Dotted(
        EnableWinsFlag(DotSet(Dots.single(nextDot))),
        current.inner.repr add nextDot
      ).mutator
    }
    def disable(using PermCausalMutate)(): C = {
      Dotted(
        EnableWinsFlag(DotSet(Dots.empty)),
        current.inner.repr
      ).mutator
    }
  }

}
