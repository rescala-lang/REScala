package rescala

import rescala.graph.{Spores, Reactive}
import rescala.turns.Ticket

object Infiltrator {
  final def getLevel[S <: Spores](reactive: Reactive[S])(implicit maybe: Ticket[S]) = maybe { reactive.bud.level(_) }
}
