package rescala.propagation

import scala.language.implicitConversions

final case class MaybeTurn(self: Either[Turn, TurnFactory]) extends AnyVal {
  def apply[T](f: Turn => T): T = self match {
    case Left(turn) => f(turn)
    case Right(factory) => factory.maybeDynamicTurn(f)
  }
}

object MaybeTurn extends LowPriorityMaybeTurn {
  implicit def explicit(implicit turn: Turn): MaybeTurn = MaybeTurn(Left(turn))
}

sealed trait LowPriorityMaybeTurn {
  implicit def dynamic(implicit factory: TurnFactory): MaybeTurn = MaybeTurn(Right(factory))
}
