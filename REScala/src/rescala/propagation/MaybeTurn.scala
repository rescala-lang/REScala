package rescala.propagation

case class MaybeTurn(turn: Option[Turn])

object MaybeTurn extends LowPrioMaybeTurn {
  implicit def explicit(implicit turn: Turn): MaybeTurn = MaybeTurn(Some(turn))
}

trait LowPrioMaybeTurn {
  implicit def dynamic: MaybeTurn = MaybeTurn(Turn.currentTurn.value)
}
