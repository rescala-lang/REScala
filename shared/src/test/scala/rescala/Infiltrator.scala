package rescala

import rescala.graph.Reactive
import rescala.turns.Ticket

object Infiltrator {
  final def getLevel(reactive: Reactive)(implicit maybe: Ticket) = maybe { reactive.level.get(_) }
}
