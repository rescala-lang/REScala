package rescala

import rescala.graph.{State, Reactive}
import rescala.turns.Ticket

object Infiltrator {
  final def getLevel[S <: State](reactive: Reactive[S])(implicit maybe: Ticket[S]) = maybe { reactive.level.get(_) }
}
