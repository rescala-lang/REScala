package rescala

import rescala.graph.Reactive
import rescala.turns.Ticket
import rescala.turns.Turn

object Infiltrator {
  final def getLevel(reactive: Reactive)(implicit maybe: Ticket) = maybe { t : Turn => reactive.level(t).get(t) }
}
