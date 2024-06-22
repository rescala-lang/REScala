package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice}
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.{LocalUid, OpsSyntaxHelper}
import rdts.time.Dots

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
case class EnableWinsFlag(inner: Dots) derives Bottom {

  type Delta = Dotted[EnableWinsFlag]

  def read: Boolean = !inner.dots.isEmpty

  def enable(using LocalUid)()(using context: Dots): Delta = {
    val nextDot = context.nextDot(LocalUid.replicaId)
    Dotted(
      EnableWinsFlag(Dots.single(nextDot)),
      inner add nextDot
    )
  }

  def disable(): Delta = {
    Dotted(
      EnableWinsFlag(Dots.empty),
      inner
    )
  }
}

object EnableWinsFlag {

  given lattice: Lattice[EnableWinsFlag]    = Lattice.derived
  given hasDotsEWF: HasDots[EnableWinsFlag] = HasDots.derived

  val empty: EnableWinsFlag = EnableWinsFlag(Dots.empty)

}
