package rescala.propagation

final class MaybeTurn(val turn: Option[Turn]) extends AnyVal

object MaybeTurn extends LowPriorityMaybeTurn {
  implicit def explicit(implicit turn: Turn): MaybeTurn = new MaybeTurn(Some(turn))
}

sealed trait LowPriorityMaybeTurn {
  implicit def dynamic: MaybeTurn = new MaybeTurn(Turn.currentTurn.value)
}
