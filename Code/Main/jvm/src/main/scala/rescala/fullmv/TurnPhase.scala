package rescala.fullmv

object TurnPhase {
  type Type = Int
  val dummy: Type = 0

  val Uninitialized: Type = 1
  val Framing: Type       = 2
  val Executing: Type     = 3
  val Completed: Type     = 4

  def toString(phase: TurnPhase.Type) =
    phase match {
      case 1 => "Uninitialized"
      case 2 => "Framing"
      case 3 => "Executing"
      case 4 => "Completed"
      case _ => s"unkonwn($phase)"
    }
}
