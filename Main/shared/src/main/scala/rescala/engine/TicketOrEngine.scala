package rescala.engine

import rescala.graph.{OutsidePropagationTicket, Struct}

object TicketOrEngine extends LowPriorityTicketOrEngine {
  implicit def fromTicket[S <: Struct](implicit ticket: OutsidePropagationTicket[S]): TicketOrEngine[S] = TicketOrEngine(Left(ticket))
}
sealed trait LowPriorityTicketOrEngine {
  implicit def fromEngine[S <: Struct](implicit engine: Engine[S, _ <: Turn[S]]): TicketOrEngine[S] = TicketOrEngine(Right(engine))
}
case class TicketOrEngine[S <: Struct](e: Either[OutsidePropagationTicket[S], Engine[S, _ <: Turn[S]]])
