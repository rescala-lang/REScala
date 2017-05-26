package rescala.fullmv

trait TurnPhase {
  def phase: TurnPhase.Type
}

object TurnPhase {
  type Type = Int
  val Initialized: Type = 0
  val Framing: Type = 1
  val Executing: Type = 2
  val WrapUp: Type = 3
  val Completed: Type = 4
}
