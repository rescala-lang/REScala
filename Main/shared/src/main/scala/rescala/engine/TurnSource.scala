package rescala.engine

import rescala.graph.{AlwaysTicket, Struct}

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

/**
  * A turn source that stores a turn/engine and can be applied to a function to call it with the appropriate turn as parameter.
  *
  * @param self Turn or engine stored by the turn source
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
@implicitNotFound(msg = "could not generate a turn source." +
  " An available implicit Ticket will serve as turn source, or if no" +
  " such turn is present, an implicit Engine is accepted instead.")
final case class TurnSource[S <: Struct](self: Either[Turn[S], Engine[S, Turn[S]]]) extends AnyVal {
  def requireCurrentTurn: Option[Turn[S]] = self match {
    case Left(turn) => Some(turn)
    case Right(engine) => engine.currentTurn()
  }
  def apply[T](f: Turn[S] => T): T = self match {
    case Left(turn) => f(turn)
    case Right(engine) => engine.currentTurn() match {
      case Some(turn) => f(turn)
      case None => engine.executeTurn(Set.empty, ticket => f(ticket.turn), engine.noWrapUp[T])
    }
  }
}


object TurnSource extends LowPriorityTurnSource {
  implicit def fromTicketImplicit[S <: Struct](implicit ticket: AlwaysTicket[S]): TurnSource[S] = TurnSource(Left(ticket.turn))
  implicit def fromTicket[S <: Struct](ticket: AlwaysTicket[S]): TurnSource[S] = TurnSource(Left(ticket.turn))
}
sealed trait LowPriorityTurnSource {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Engine[S, Turn[S]]): TurnSource[S] = TurnSource(Right(factory))
  implicit def fromEngine[S <: Struct](factory: Engine[S, Turn[S]]): TurnSource[S] = TurnSource(Right(factory))
}
