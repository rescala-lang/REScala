package rescala.propagation.turns.creation

import rescala.propagation.Engine
import rescala.propagation.turns.Turn

import scala.language.implicitConversions

final case class Ticket(self: Either[Turn, Engine]) extends AnyVal {
  def apply[T](f: Turn => T): T = self match {
    case Left(turn) => f(turn)
    case Right(factory) => factory.startKeep(f)
  }
}

object Ticket extends LowPriorityTicketImplicits {
  implicit def explicit(implicit turn: Turn): Ticket = Ticket(Left(turn))
}

sealed trait LowPriorityTicketImplicits {
  implicit def dynamic(implicit factory: Engine): Ticket = Ticket(Right(factory))
}
