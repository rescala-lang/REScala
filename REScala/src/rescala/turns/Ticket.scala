package rescala.turns

import scala.language.implicitConversions

final case class Ticket(self: Either[Turn, Engine]) extends AnyVal {
  def apply[T](f: Turn => T): T = self match {
    case Left(turn) => f(turn)
    case Right(factory) => factory.subplan(f)
  }
}

object Ticket extends LowPriorityTicket {
  implicit def explicit(implicit turn: Turn): Ticket = Ticket(Left(turn))
}

sealed trait LowPriorityTicket {
  implicit def dynamic(implicit factory: Engine): Ticket = Ticket(Right(factory))
}
