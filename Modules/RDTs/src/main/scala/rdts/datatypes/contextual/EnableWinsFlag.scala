package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice}
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.{LocalReplicaId, OpsSyntaxHelper}
import rdts.time.Dots

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
case class EnableWinsFlag(inner: Dots) derives Bottom

object EnableWinsFlag {

  given lattice: Lattice[EnableWinsFlag]    = Lattice.derived
  given hasDotsEWF: HasDots[EnableWinsFlag] = HasDots.derived

  val empty: EnableWinsFlag = EnableWinsFlag(Dots.empty)

  extension [C](container: C)
    def enableWinsFlag: syntax[C] = syntax(container)

  /** It is enabled if there is a value in the store.
    * It relies on the external context to track removals.
    */
  implicit class syntax[C](container: C) extends OpsSyntaxHelper[C, EnableWinsFlag](container) {
    def read(using IsQuery): Boolean = !current.dots.isEmpty

    def enable(using LocalReplicaId)(): CausalMutator = {
      val nextDot = context.nextDot(replicaId)
      Dotted(
        EnableWinsFlag(Dots.single(nextDot)),
        current.dots add nextDot
      ).mutator
    }
    def disable(using IsCausalMutator)(): C = {
      Dotted(
        EnableWinsFlag(Dots.empty),
        current.dots
      ).mutator
    }
  }

}
