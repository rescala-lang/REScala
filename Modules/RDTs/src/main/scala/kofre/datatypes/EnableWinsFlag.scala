package kofre.datatypes

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.Epoche
import kofre.dotted.{DotSet, Dotted, DottedLattice, HasDots}
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dots

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
case class EnableWinsFlag(inner: DotSet) derives Bottom

object EnableWinsFlag {

  given contextDecompose: DottedLattice[EnableWinsFlag] = DottedLattice.derived
  given hasDotsEWF: HasDots[EnableWinsFlag] with {
    override def getDots(a: EnableWinsFlag): Dots = a.inner.dots
  }

  val empty: EnableWinsFlag = EnableWinsFlag(DotSet.empty)

  extension [C](container: C)
    def enableWinsFlag: syntax[C] = syntax(container)

  /** It is enabled if there is a value in the store.
    * It relies on the external context to track removals.
    */
  implicit class syntax[C](container: C) extends OpsSyntaxHelper[C, EnableWinsFlag](container) {
    def read(using PermQuery): Boolean = !current.inner.dots.isEmpty

    def enable(using ReplicaId, PermCausalMutate)(): C = {
      val nextDot = context.nextDot(replicaId)
      Dotted(
        EnableWinsFlag(DotSet(Dots.single(nextDot))),
        current.inner.dots add nextDot
      ).mutator
    }
    def disable(using PermCausalMutate)(): C = {
      Dotted(
        EnableWinsFlag(DotSet(Dots.empty)),
        current.inner.dots
      ).mutator
    }
  }

}
