package rescala.engine

import rescala.graph.Struct
import rescala.propagation.{DynamicTicket, StaticTicket, Turn}

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

/**
  * A turn source that stores a turn/engine and can be applied to a function to call it with the appropriate turn as parameter.
  *
  * @param self Turn or engine stored by the turn source
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
@implicitNotFound(msg = "could not generate a turn source." +
  " An available implicit Turn will serve as turn source, or if no" +
  " such turn is present, an implicit Engine is accepted instead.")
final case class TurnSource[S <: Struct](self: Either[Turn[S], Engine[S, Turn[S]]]) extends AnyVal {
  def apply[T](f: Turn[S] => T): T = self match {
    case Left(turn) => f(turn)
    case Right(factory) => factory.subplan()(f)
  }
}

object TurnSource extends LowPriorityTurnSource {
  implicit def fromTurnImplicit[S <: Struct](implicit turn: Turn[S]): TurnSource[S] = TurnSource(Left(turn))
  implicit def fromTurn[S <: Struct](turn: Turn[S]): TurnSource[S] = TurnSource(Left(turn))
  implicit def fromStaticTicketImplicit[S <: Struct](implicit ticket: StaticTicket[S]): TurnSource[S] = TurnSource(Left(ticket.turn))
  implicit def fromStaticTicket[S <: Struct](ticket: StaticTicket[S]): TurnSource[S] = TurnSource(Left(ticket.turn))
  implicit def fromDynamicTicketImplicit[S <: Struct](implicit ticket: DynamicTicket[S]): TurnSource[S] = TurnSource(Left(ticket.turn))
  implicit def fromDynamicTicket[S <: Struct](ticket: DynamicTicket[S]): TurnSource[S] = TurnSource(Left(ticket.turn))
}

sealed trait LowPriorityTurnSource {
  implicit def fromEngineImplicit[S <: Struct](implicit factory: Engine[S, Turn[S]]): TurnSource[S] = TurnSource(Right(factory))
  implicit def fromEngine[S <: Struct](factory: Engine[S, Turn[S]]): TurnSource[S] = TurnSource(Right(factory))
}
