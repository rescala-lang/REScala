package rescala

import rescala.engines.Ticket
import rescala.graph.{Reactive, Spores}

object Infiltrator {
  final def getLevel[S <: Spores](reactive: Reactive[S])(implicit maybe: Ticket[S]) = maybe { reactive.bud.level(_) }
}
