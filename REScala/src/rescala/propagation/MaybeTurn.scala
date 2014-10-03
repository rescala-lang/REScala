package rescala.propagation

final class MaybeTurn(val turn: Option[Turn]) extends AnyVal

object MaybeTurn extends LowPrioMaybeTurn {
  implicit def explicit(implicit turn: Turn): MaybeTurn = new MaybeTurn(Some(turn))
}

trait LowPrioMaybeTurn {
  implicit def dynamic: MaybeTurn = new MaybeTurn(Turn.currentTurn.value)
}
