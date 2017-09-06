package rescala.fullmv

object TurnPhase {
  type Type = Int
  val dummy: Type = 0

  val Initialized: Type = 1
  val Framing: Type = 2
  val Executing: Type = 3
  val WrapUp: Type = 4
  val Completed: Type = 5

  def toString(phase: TurnPhase.Type) = phase match {
    case 1 => "Initialized"
    case 2 => "Framing"
    case 3 => "Executing"
    case 4 => "WrapUp"
    case 5 => "Completed"
  }
}
