package rescala.turns

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

@implicitNotFound(msg = "could not generate a ticket." +
  " tickets are available whenever an implicit turn is available" +
  " (in which case that turn is used) or if no turn is present," +
  " an implicit engine will be used to generate new tickets")
final case class Ticket(self: Either[Turn, Engine[Turn]]) extends AnyVal {
  def apply[T](f: Turn => T): T = self match {
    case Left(turn) => f(turn)
    case Right(factory) => factory.subplan()(f)
  }
}

object Ticket extends LowPriorityTicket {
  implicit def explicit(implicit turn: Turn): Ticket = Ticket(Left(turn))
}

sealed trait LowPriorityTicket {
  implicit def dynamic(implicit factory: Engine[Turn]): Ticket = Ticket(Right(factory))
}
